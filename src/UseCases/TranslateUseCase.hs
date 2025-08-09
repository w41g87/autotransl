module UseCases.TranslateUseCase where

import Network.Wreq
import Calamity
import DiPolysemy
import Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding    as TL
import Data.Default
import Data.Coerce
import Data.Aeson (encode)
import qualified Polysemy as P
import qualified Data.Vector as V

import Domains.Domain
import UseCases.API
import Domains.MessageQueue

type TransCall = ApiCall TransRequest TransResponse

translate :: (P.Members '[TransCall, LogEff] r)
            => Text
            -> Options
            -> Text
            -> Maybe MessageQueue
            -> Text
            -> P.Sem r (TransRequest, Maybe Text)
translate base_url opts lang q txt = do
    debug $ TL.concat ["Translate request body: ", TL.decodeUtf8 $ encode req]
    translate' base_url opts req
    where
        req = def {
            text = [txt],
            target_lang = lang,
            context = coerce (T.intercalate "\n" . V.toList . V.reverse) <$> q
        } :: TransRequest

translate' :: (P.Members '[TransCall, LogEff] r)
            => Text
            -> Options
            -> TransRequest
            -> P.Sem r (TransRequest, Maybe Text)
translate' base_url opts req = do
    rsp <- postApi @TransRequest @TransResponse base_url opts req
    debug $ TL.concat ["Translate request body: ", TL.decodeUtf8 $ encode req]
    return (req, rsp >>= toPara (target_lang req))