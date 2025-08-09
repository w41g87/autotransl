module InterfaceAdapters.Config where

import Data.Text
import Optics
import GHC.Generics
import Polysemy
import System.Environment (getEnv)
-- | global application configuration

data Config = Config {
    _host :: Text,
    _port :: Int,
    _db :: Text,
    _user :: Text,
    _pass :: Text
} deriving (Show, Eq, Read, Generic)

makeLenses ''Config

envConfig :: Member (Embed IO) r
            => Sem r Config
envConfig = do
    let host = "localhost"
    let port = 5432
    db <- embed $ getEnv "POSTGRES_DB"
    user <- embed $ getEnv "POSTGRES_USER"
    pass <- embed $ getEnv "POSTGRES_PASSWORD"
    return $ Config host port (pack db) (pack user) (pack pass)
