module Main (main) where

import      Calamity
import      Calamity.Cache.InMemory
import      Calamity.Commands
import      Calamity.Commands.Context
import      Calamity.Commands.Dsl as DSL
import      Calamity.Metrics.Noop
import      Calamity.Cache.Eff
import      Calamity.Metrics.Eff
import      Control.Monad as M
import      Optics
import      Optics.Core.Extras
import      Data.Text as T
import      DiPolysemy as DP
import qualified Polysemy as P
import      Polysemy.Input
import      Polysemy.Error
import qualified Di
import      System.Environment (lookupEnv)
import      Data.LanguageCodes

import Domains.Domain
import Domains.MessageQueue
import InterfaceAdapters.KVSPostgre
import InterfaceAdapters.APIHttp
import InterfaceAdapters.Config
import UseCases.Commands
import UseCases.Translate
import UseCases.KVS

readTokenRunBotIO :: P.Members '[P.Embed IO, P.Final IO, CacheEff, MetricEff, LogEff] r =>
    Intents -> P.Sem (SetupEff r) a -> P.Sem r (Maybe StartupError)
readTokenRunBotIO int program = do
    t <- P.embed $ lookupEnv "BOT_TOKEN"
    case t of
        Just t -> do
            info @Text "Bot token loaded"
            runBotIO (BotToken $ pack t) int program
        Nothing -> do
            DP.error @Text "Bot token not found in env!"
            pure $ Just $ StartupError "Bot token not found in env!"


main :: IO ()
main = Di.new $ \di -> do
    result <-
        P.runFinal
        . P.embedToFinal @IO
        . errorToIOFinal
        . runDiToIO di
        . runCacheInMemory
        . runMetricsNoop
        . runInputSem envConfig
        . runKvsPostgre @_ @ApiToken
        . runKvsPostgre @_ @ISO639_1
        . runKvsPostgre @_ @MessageQueue
        . runKvsPostgre @_ @TransRequest
        . runKvsPostgre @_ @(Snowflake Message)
        . runAPICall @_ @DetectRequest @DetectResponse
        . runAPICall @_ @TransRequest @TransResponse
        . useFullContext
        . useConstantPrefix "/"
        . readTokenRunBotIO defaultIntents
        $ do
            info @Text "Initializing Database..."
            runInputSem envConfig dbInit

            info @Text "Setting up commands and handlers..."
            addCommands $ do
                helpCommand
                DSL.command @'[] "setup" addToken
                DSL.command @'[Text] "autotranslate" setLang
                DSL.command @'[] "autotranslate_off" removeLang

            react @'MessageCreateEvt $ \(m, usr, _member) -> do
                unless (is (_Just % #bot % _Just % only True) usr) $ do
                    info $ T.concat ["Received message: ", m ^. #content]
                    case m ^. #guildID of
                        Just gld -> do
                            result <- targetedTr gld m
                            case result of
                                Just txt -> do
                                    msg <- reply m txt
                                    case msg of
                                        Left e -> DP.error $ show e
                                        Right msg -> insertKvs ReplyStore (getID @Message m) (getID @Message msg)
                                Nothing -> return ()
                        _ -> return ()

            react @'MessageUpdateEvt $ \(b4, af, usr, _member) -> do
                msg_id <- getKvs ReplyStore (getID @Message b4)
                case msg_id of
                    -- Not bot user and actual revision
                    Just msg_id -> unless (is (_Just % #bot % _Just % only True) usr && (b4 ^. #content) /= (af ^. #content)) $ do
                        info $ T.concat ["Message edited from: ", b4 ^. #content, " to: ", af ^. #content]
                        old_msg <- getMessage msg_id
                        case (af ^. #guildID, old_msg) of
                            (Just gld, Just old_msg) -> do
                                result <- reviseTr gld af
                                new_msg <- invoke $ EditMessage old_msg old_msg (editMessageContent result)
                                case new_msg of
                                    Left e -> DP.error $ show e
                                    Right new_msg -> insertKvs ReplyStore (getID @Message af) (getID @Message new_msg)
                            _ -> return ()
                    Nothing -> return ()

            react @'RawMessageDeleteEvt $ \msg -> do
                msg_id <- getKvs ReplyStore msg
                tr_msg <- M.join <$> traverse getMessage msg_id
                case tr_msg of
                    -- Found linked message
                    Just tr_msg -> do
                        info $ "Message Deleted: " ++ show msg ++ " corresponding to id: " ++ maybe "" show msg_id
                        deleteKvs ReplyStore msg
                        void $ invoke $ DeleteMessage tr_msg tr_msg
                    _ -> return ()

    case result of
        -- Left x -> print x
        Right (Just y) -> print y
        _ -> pure ()



