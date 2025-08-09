{-# LANGUAGE ApplicativeDo #-}
module UseCases.Commands where

import Data.Functor
import Data.Text
import Control.Monad

import Calamity
import Calamity.Interactions as CI
import Calamity.Commands.Context
import Polysemy as P
import Polysemy.Async
import DiPolysemy as DP
import Optics
import Data.Flags
import Data.LanguageCodes

import UseCases.KVS
import InterfaceAdapters.KVSPostgre

addToken :: BotC r
        => (P.Members '[LogEff, TokenStore] r)
        => FullContext
        -> Sem r ()
addToken ctx = case (ctx ^. #guild, ctx ^. #member) of
    (Just guild, Just member) -> do
        let perm = basePermissions guild member
            intro = do
                    a <- row $ do
                            a <- CI.button ButtonPrimary "Detection API Token"
                            b <- CI.button ButtonPrimary "Translation API Token"
                            pure (a, b)
                    d <- CI.button ButtonSuccess "Done"
                    pure (a, d)

            detection_form = CI.textInput TextInputShort "Detection API Token"
            translation_form = CI.textInput TextInputShort "Translation API Token"

        info $ show perm ++ show member
        when (perm `containsSome` (manageGuild .+. administrator)) $ do
            runView intro (tell ctx) $ \((a, b), d) -> do
                int <- getInteraction
                case (int ^. #guildID, int ^. #member) of
                    (Just guild_id, Just member) -> do
                        guild <- upgrade guild_id
                        case guild of
                            Just guild -> do
                                let perm = basePermissions guild member
                                when a $ do
                                    if perm `containsSome` (manageGuild .+. administrator)
                                        then void . async $ do
                                            runView detection_form (void . pushModal "Detection") $ \a -> do
                                                info $ "DetectToken update for " ++ maybe "" (show . getID @Guild) (ctx ^. #guild)
                                                insertKvs DetectToken (getID @Guild guild) a
                                                b <- getKvs DetectToken (getID @Guild guild)
                                                case b of 
                                                    Just b -> do 
                                                        void $ respondEphemeral ("Detection Token Updated: " <> b )
                                                        endView ()
                                                    Nothing -> endView ()
                                                endView ()
                                        else void $ respondEphemeral @Text "Insufficient permission to edit API token!"

                                when b $ do
                                    if perm `containsSome` (manageGuild .+. administrator)
                                        then void . async $ do
                                            runView translation_form (void . pushModal "Translation") $ \a -> do
                                                info $ "TransToken update for " ++ maybe "" (show . getID @Guild) (ctx ^. #guild)
                                                insertKvs TransToken (getID @Guild guild) a
                                                b <- getKvs TransToken (getID @Guild guild)
                                                case b of 
                                                    Just b -> do 
                                                        void $ respondEphemeral ("Translation Token Updated: " <> b )
                                                        endView ()
                                                    Nothing -> endView ()
                                        else void $ respondEphemeral @Text "Insufficient permission to edit API token!"

                                when d $ do
                                    deleteInitialMsg
                                    endView ()
                            Nothing -> void $ respondEphemeral @Text "You should not be seeing this"
                    _ -> do
                        void $ respondEphemeral @Text "You should not be seeing this"

    _ -> return ()


setLang :: BotC r
        => (P.Members '[LogEff, KVS DataStore (Snowflake Channel) ISO639_1] r)
        => FullContext
        -> Text
        -> Sem r ()
setLang ctx lang = case (ctx ^. #guild, ctx ^. #member) of
    (Just guild, Just member) -> do
        let perm = basePermissions guild member
        
        when (perm `containsSome` (manageChannels .+. manageGuild .+. administrator)) $ do
            case reads @ISO639_1 (unpack $ toUpper lang) of
                [(value, "")] -> do
                    insertKvs LangStore (getID @Channel (ctx ^. #channel)) value
                    void $ reply ctx ( pack $ "The target language of this channel has been set to " ++ show value )
                _ -> do
                    void $ reply ctx $ "\"" ++ show lang ++ "\" is not a valid ISO language code!"
    _ -> return ()

removeLang :: BotC r
        => (P.Members '[LogEff, KVS DataStore (Snowflake Channel) ISO639_1] r)
        => FullContext
        -> Sem r ()
removeLang ctx = case (ctx ^. #guild, ctx ^. #member) of
    (Just guild, Just member) -> do
        let perm = basePermissions guild member
        when (perm `containsSome` (manageChannels .+. manageGuild .+. administrator)) $ do
            deleteKvs LangStore (getID @Channel (ctx ^. #channel))
            void $ reply @Text ctx "Auto translation has been turned off for this channel."
    _ -> return ()
