module InterfaceAdapters.APIHttp where

import Polysemy
import qualified DiPolysemy as DP
import qualified Calamity as C
import UseCases.API
import Data.Aeson
import Network.Wreq
import Data.Text
import Data.Text.Encoding
import Control.Lens


runAPICall :: (Members '[Embed IO, C.LogEff] r, ToJSON a, FromJSON b)
            => Sem (ApiCall a b ': r) c
            -> Sem r c
runAPICall = interpret $ \case
    GetApi url opts -> do
        rsp <- embed $ getWith opts $ unpack url
        if rsp ^. responseStatus . statusCode == 200
            then do
                json <- embed $ asJSON rsp
                return $ Just (json ^. responseBody)
            else do
                DP.error $ decodeUtf8 (rsp ^. responseStatus . statusMessage)
                return Nothing
    PostApi url opts body -> do
        rsp <- embed $ postWith opts (unpack url) (toJSON body)
        if rsp ^. responseStatus . statusCode == 200
            then do
                json <- embed $ asJSON rsp
                return $ Just (json ^. responseBody)
            else do
                DP.error $ decodeUtf8 (rsp ^. responseStatus . statusMessage)
                return Nothing