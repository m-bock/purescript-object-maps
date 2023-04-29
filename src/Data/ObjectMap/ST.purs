module Data.ObjectMap.ST where

import Prelude

import Control.Monad.ST (ST)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Maybe (Maybe)
import Data.ObjectMap as OM
import Data.Tuple (Tuple, snd)
import Data.Tuple.Nested ((/\))
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as STO
import Foreign.Object.ST.Unsafe as STOU

newtype STObjectMap r k v = STObjectMap (STObject r (Tuple k v))

-- | Create a new, empty mutable map
new :: forall r k v. ST r (STObjectMap r k v)
new = STObjectMap <$> STO.new

-- | Get the value for a key in a mutable map
peek :: forall r k v. EncodeJson k => k -> STObjectMap r k v -> ST r (Maybe v)
peek k (STObjectMap m) = map snd <$> STO.peek (OM.toJsonStr k) m

-- | Update the value for a key in a mutable map
poke :: forall r k v. EncodeJson k => k -> v -> STObjectMap r k v -> ST r (STObjectMap r k v)
poke k v (STObjectMap m) = STObjectMap <$> STO.poke (OM.toJsonStr k) (k /\ v) m

-- | Remove a key and the corresponding value from a mutable map
delete :: forall r k v. EncodeJson k => k -> STObjectMap r k v -> ST r (STObjectMap r k v)
delete k (STObjectMap m) = STObjectMap <$> STO.delete (OM.toJsonStr k) m

-- | Unsafely get the map out of ST without copying it
unsafeFreeze :: forall r k v. STObjectMap r k v -> ST r (OM.ObjectMap k v)
unsafeFreeze (STObjectMap m) = OM.ObjectMap <$> STOU.unsafeFreeze m