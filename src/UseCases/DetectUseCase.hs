module UseCases.DetectUseCase where

import Network.Wreq
import Data.Text
import qualified Polysemy as P
-- import qualified DiPolysemy as DiP

import Domains.Domain
import UseCases.API

type DetectCall = ApiCall DetectRequest DetectResponse

detect :: (P.Members '[DetectCall] r) => Text -> Options -> Text -> Text -> P.Sem r Bool
detect base_url opts lang txt = do
    rsp <- postApi @DetectRequest @DetectResponse base_url opts (DetectRequest txt)
    return $ maybe False (isTarget lang) rsp
