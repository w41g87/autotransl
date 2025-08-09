{-# LANGUAGE StandaloneDeriving #-}
module InterfaceAdapters.KVSPostgre where

import Polysemy
import Polysemy.Input
import Polysemy.Fail
import qualified DiPolysemy as DP
import qualified Calamity as C
import Calamity.Types.Snowflake
import UseCases.KVS
import qualified Hasql.Statement as ST
import qualified Hasql.Session as SE
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Setting as ConnectionSetting
import qualified Hasql.Connection.Setting.Connection as ConnectionSettingConnection
import qualified Data.Vector as V
import Optics
import Data.Int
import Data.Text
import Data.Text.Encoding
import Data.Functor.Contravariant
import GHC.Generics
import Data.LanguageCodes

import Domains.Domain
import InterfaceAdapters.Config
import Data.Aeson

deriving instance Generic ISO639_1
instance FromJSON ISO639_1
instance ToJSON ISO639_1

type TokenStore = KVS DataStore (Snowflake C.Guild) ApiToken

getStmt :: Text -> ST.Statement Int64 (Maybe Value)
getStmt store = ST.Statement sql encoder decoder True where
    sql = encodeUtf8 . pack $ "select token from " ++ unpack store ++ " where id = $1"
    encoder =
        Encoders.param (Encoders.nonNullable Encoders.int8)
    decoder = Decoders.rowMaybe (Decoders.column (Decoders.nonNullable Decoders.jsonb))

insertStmt :: Text -> ST.Statement (Int64, Value) ()
insertStmt store = ST.Statement sql encoder decoder True where
    sql = encodeUtf8 . pack $ "insert into " ++ unpack store ++ " (id, token) values ($1, $2) \
    \ON CONFLICT (id)\
    \DO UPDATE SET\
    \    token = EXCLUDED.token"
    encoder =
        (fst >$< Encoders.param (Encoders.nonNullable Encoders.int8)) <>
        (snd >$< Encoders.param (Encoders.nonNullable Encoders.jsonb))
    decoder = Decoders.noResult

listStmt :: Text -> ST.Statement () (V.Vector (Int64, Value))
listStmt store = ST.Statement sql encoder decoder True where
    sql = encodeUtf8 . pack $ "select id, token from " ++ unpack store
    encoder = Encoders.noParams
    decoder = Decoders.rowVector (
        (,) <$>
        Decoders.column (Decoders.nonNullable Decoders.int8) <*>
        Decoders.column (Decoders.nonNullable Decoders.jsonb))

deleteStmt :: Text -> ST.Statement Int64 ()
deleteStmt store = ST.Statement sql encoder decoder True where
    sql = encodeUtf8 . pack $ "delete from " ++ unpack store ++ " where id = $1"
    encoder = Encoders.param (Encoders.nonNullable Encoders.int8)
    decoder = Decoders.noResult


runGet :: (Members '[Embed IO, C.LogEff, Input Config] r)
                => a
                -> ST.Statement a (Maybe b)
                -> Sem r (Maybe b)
runGet k stmt = do
    conn <- getConnection input
    case conn of
        Just x -> do
            rsp <- embed $ SE.run (SE.statement k stmt) x
            case rsp of
                Left x -> do
                    DP.info $ show x
                    return Nothing
                Right y -> return y
        Nothing -> return Nothing

runInsert :: (Members '[Embed IO, C.LogEff, Input Config] r)
            => ToJSON b
            => a
            -> b
            -> ST.Statement (a, Value) ()
            -> Sem r ()
runInsert k v stmt = runStmt (SE.statement (k, toJSON v) stmt)

runList :: (Members '[Embed IO, C.LogEff, Input Config] r)
                => ST.Statement () (V.Vector (a, b))
                -> Sem r (V.Vector (a, b))
runList stmt = runStmt (SE.statement () stmt)

runDelete :: (Members '[Embed IO, C.LogEff, Input Config] r)
                => a
                -> ST.Statement a ()
                -> Sem r ()
runDelete k stmt = runStmt (SE.statement k stmt)


tryParseJSON :: forall a. forall r. (Members '[C.LogEff] r, FromJSON a)
            => Value
            -> Sem r (Maybe a)
tryParseJSON v =
    case fromJSON v :: Result a of
        Success value -> return $ Just value
        Error err -> do
            DP.error err
            return Nothing

runKvsPostgre :: (Members '[Embed IO, C.LogEff, Input Config] r)
                => (FromJSON c, ToJSON c)
                => Sem (KVS DataStore (Snowflake b) c ': r) a
                -> Sem r a
runKvsPostgre = interpret $ \case
    GetKvs s k      -> do
            result <- runGet (fromIntegral $ fromSnowflake k) (getStmt (pack . show $ s))
            case result of
                Just v ->
                    tryParseJSON v
                Nothing -> do
                    DP.info $ "Empty row for id " ++ show k 
                    return Nothing
    ListAllKvs s    -> do
        result <- runList (listStmt $ pack . show $ s)
        let go (x, y) acc = do
                res <- tryParseJSON y
                _acc <- acc
                case res of
                    Just y -> return $ V.cons (Snowflake $ fromIntegral x, y) _acc
                    Nothing -> acc
        Prelude.foldr go mempty result

    InsertKvs s k v -> runInsert (fromIntegral $ fromSnowflake k) v (insertStmt $ pack . show $ s)
    DeleteKvs s k   -> runDelete (fromIntegral $ fromSnowflake k) (deleteStmt $ pack . show $ s)

-- runKvsPostgre :: (Members '[Embed IO, C.LogEff, Input Config] r)
--                 => Sem (TokenStore ': r) a
--                 -> Sem r a
-- runKvsPostgre = interpret $ \case
--     GetKvs s k      -> runGet (fromIntegral $ fromSnowflake k) (getStmt (pack . show $ s))
--     ListAllKvs s    -> let result = runList (listStmt $ pack . show $ s)
--                         in fmap (first (Snowflake . fromIntegral)) <$> result
--     InsertKvs s k v -> runInsert (fromIntegral $ fromSnowflake k) v (insertStmt $ pack . show $ s)
--     DeleteKvs s k   -> runDelete (fromIntegral $ fromSnowflake k) (deleteStmt $ pack . show $ s)

getConnection :: (Members '[Embed IO, C.LogEff] r)
                => Sem r Config
                -> Sem r (Maybe Connection.Connection)
getConnection config = do
    c <- config
    conn <- embed $ sqlConn c
    case conn of
        Left x -> case x of
            Just x -> do
                DP.error $ show x
                return Nothing
            Nothing ->
                return Nothing
        Right connection -> return $ Just connection
    where
        sqlConn :: Config -> IO (Either Connection.ConnectionError Connection.Connection)
        sqlConn x = Connection.acquire settings
            where
                settings = [ConnectionSetting.connection $ ConnectionSettingConnection.string pstr]
                pstr = pack $
                    "host=" ++ unpack (x ^. host)
                    ++ " dbname=" ++ unpack (x ^. db)
                    ++ " user=" ++ unpack (x ^. user)
                    ++ " password=" ++ unpack (x ^. pass)
                    ++ " port=" ++ show (x ^. port)

runStmt :: (Members '[Embed IO, C.LogEff, Input Config] r)
        => Monoid b
        => SE.Session b
        -> Sem r b
runStmt stmt = do
    conn <- getConnection input
    case conn of
        Just x -> do
            rsp <- embed $ SE.run stmt x
            case rsp of
                Left x -> do
                    DP.info $ show x
                    return mempty
                Right y -> return y
        Nothing -> return mempty

dbInit :: (Members '[Embed IO, C.LogEff, Input Config] r)
        => Sem r ()
dbInit = do
    result <- traverse (runStmt . stmt) [minBound .. maxBound @DataStore]
    return $ mconcat result
    where
        stmt store = SE.statement () $ ST.Statement (encodeUtf8 . pack $
            "CREATE TABLE IF NOT EXISTS " ++ show store ++ " (\
            \    id BIGINT PRIMARY KEY,\
            \    token JSONB\
            \);")
            Encoders.noParams
            Decoders.noResult
            False