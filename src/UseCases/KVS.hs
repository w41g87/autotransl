module UseCases.KVS where

import Polysemy
import Optics
import qualified Data.Vector as V
import GHC.Generics


data DataStore = DetectToken 
                | TransToken 
                | LangStore 
                | ContextStore 
                | HistoryStore 
                | ReplyStore
  deriving (Show, Eq, Read, Generic, Enum, Bounded)
makePrisms ''DataStore

-- | a key value store specified as A GADT type
data KVS s k v m a where
  ListAllKvs :: s -> KVS s k v m (V.Vector (k, v))
  GetKvs     :: s -> k -> KVS s k v m (Maybe v)
  InsertKvs  :: s -> k -> v -> KVS s k v m ()
  DeleteKvs  :: s -> k -> KVS s k v m ()

-- | makeSem uses TemplateHaskell to generate effect functions (or smart Constructors) from the GADT definition:
-- listAllKvs :: Member (KVS k v) r => Sem r [(k, v)]
-- getKvs     :: Member (KVS k v) r => k -> Sem r (Maybe v)
-- insertKvs  :: Member (KVS k v) r => k -> v -> Sem r ()
-- deleteKvs  :: Member (KVS k v) r => k -> Sem r ()
makeSem ''KVS
