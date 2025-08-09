module UseCases.API where

import Polysemy
import Data.Text
import Network.Wreq

data ApiCall a b m c where
    GetApi :: Text -> Options -> ApiCall a b m (Maybe b)
    PostApi :: Text -> Options -> a -> ApiCall a b m (Maybe b)

makeSem ''ApiCall