module UseCases.MessageCache where

import Data.Text as T
import Data.Coerce
import Control.Monad.State.Lazy
import Polysemy as P
import Calamity
import DiPolysemy
import qualified Data.Vector as V

import UseCases.KVS
import Domains.MessageQueue


cacheMessage :: Integral a
            => P.Member (KVS DataStore (Snowflake Channel) MessageQueue) r
            => P.Member LogEff r
            => a
            -> Snowflake Channel
            -> Text
            -> Sem r ()
cacheMessage n ch txt = do
    msgq <- getKvs ContextStore ch
    case msgq of
        Just (MessageQueue q) ->
            if fromIntegral (V.length q) >= n then do
                let (_, (_, newq)) = runState (enqueue txt) <$> runState dequeue (MessageQueue q)
                debug $ T.concat ["Message History Enqueued: ", coerce (T.intercalate "\n" . V.toList) newq]
                insertKvs ContextStore ch newq
            else do
                let (_, newq) = runState (enqueue txt) (MessageQueue q)
                debug $ T.concat ["Message History Enqueued: ", coerce (T.intercalate "\n" . V.toList) newq]
                insertKvs ContextStore ch newq
        Nothing -> insertKvs ContextStore ch (MessageQueue $ V.fromList [txt])