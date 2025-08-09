module Domains.MessageQueue where

import Data.Text
import qualified Data.Vector as V
import Data.Aeson
import GHC.Generics (Generic)
import Control.Monad.State.Lazy

newtype MessageQueue = MessageQueue (V.Vector Text)
    deriving (Show, Generic)
instance FromJSON MessageQueue
instance ToJSON MessageQueue

enqueue :: Text -> State MessageQueue ()
enqueue msg = do
    (MessageQueue m) <- get
    put $ MessageQueue $ V.cons msg m
    return ()

dequeue :: State MessageQueue (Maybe Text)
dequeue = do
    (MessageQueue m) <- get
    if V.null m then return Nothing
        else do
            let lst = V.last m
            put $ MessageQueue $ V.slice 0 (V.length m - 1) m
            return $ Just lst