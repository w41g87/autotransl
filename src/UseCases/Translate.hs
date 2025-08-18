module UseCases.Translate where
import      Calamity hiding (text)
import qualified Control.Lens as L
import      Data.Text as T
import qualified Data.Vector as V
import      Data.Text.Encoding
import      Data.Coerce
import      Network.Wreq
import      DiPolysemy as DP
import qualified Polysemy as P
import      Data.LanguageCodes
import      Optics

import Domains.Domain
import Domains.MessageQueue
import UseCases.DetectUseCase
import UseCases.TranslateUseCase
import InterfaceAdapters.KVSPostgre
import UseCases.KVS
import UseCases.API
import UseCases.MessageCache

targetedTr :: (P.Member (ApiCall DetectRequest DetectResponse) r
                , P.Member (ApiCall TransRequest TransResponse) r
                , P.Member (KVS DataStore (Snowflake Channel) ISO639_1) r
                , P.Member (KVS DataStore (Snowflake Channel) MessageQueue) r
                , P.Member (KVS DataStore (Snowflake Message) TransRequest) r
                , P.Members '[P.Embed IO, TokenStore, LogEff] r)
            => Snowflake Guild
            -> Message
            -> P.Sem r (Maybe Text)

targetedTr gld msg = do
    let (msg_id, ch, in_txt) = (getID @Message msg, getID @Channel msg, msg ^. #content)
    detect_token <- getKvs DetectToken gld
    trans_token <- getKvs TransToken gld
    lang <- getKvs @_ @_ @ISO639_1 LangStore ch
    case (trans_token, lang) of
        (Just trans_token, Just lang) -> do
            let detect_txt = removeVanity in_txt
            debug $ T.concat ["Detection text is ", detect_txt]
            if T.null (removeNull detect_txt)
                then return Nothing
                else do
                    cacheMessage 20 ch in_txt
                    let txt = addIgnore . removeAt $ in_txt
                    debug $ T.concat ["Formatted text is ", txt]
                    alreadyTarget <- case detect_token of
                            Just t -> do
                                let opts = L.set (header "Authorization") ["Bearer " <> encodeUtf8 t] defaults
                                detect "https://ws.detectlanguage.com/0.2/detect" opts (pack $ show lang) detect_txt
                            Nothing -> return False
                    if alreadyTarget
                        then do
                            debug $ T.concat ["Message already in target language: ", in_txt]
                            return Nothing
                        else do
                            let opts = L.set (header "Content-Type") ["application/json"] $ L.set (header "Authorization") ["DeepL-Auth-Key " <> encodeUtf8 trans_token] defaults
                            q <- getKvs @_ @_ @MessageQueue ContextStore ch
                            (req, result) <- translate "https://api-free.deepl.com/v2/translate" opts (pack $ show lang) (fmap (coerce @(V.Vector Text -> V.Vector Text) V.tail) q) txt
                            insertKvs HistoryStore msg_id req
                            return $ removeIgnore <$> result
        _ -> return Nothing

reviseTr :: (P.Member (ApiCall TransRequest TransResponse) r
                , P.Member (KVS DataStore (Snowflake Message) TransRequest) r
                , P.Members '[P.Embed IO, TokenStore, LogEff] r)
            => Snowflake Guild
            -> Message
            -> P.Sem r (Maybe Text)
reviseTr gld msg = do
    let (msg_id, in_txt) = (getID @Message msg, msg ^. #content)
    trans_token <- getKvs TransToken gld
    req_old <- getKvs HistoryStore msg_id
    case (trans_token, req_old) of 
        (Just trans_token, Just req_old) -> do
            let opts = L.set (header "Content-Type") ["application/json"] $ L.set (header "Authorization") ["DeepL-Auth-Key " <> encodeUtf8 trans_token] defaults
                req = req_old {text = [addIgnore in_txt]}
            (req, result) <- translate' "https://api-free.deepl.com/v2/translate" opts req
            insertKvs HistoryStore msg_id req
            return $ removeIgnore <$> result
        _ -> return Nothing