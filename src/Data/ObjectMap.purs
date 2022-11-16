module Data.ObjectMap
  ( ObjectMap
  , delete
  , empty
  , fromArray
  , insert
  , lookup
  , toArray
  )
  where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Lens (lens)
import Data.Lens.AffineTraversal (affineTraversal)
import Data.Lens.At (class At)
import Data.Lens.Index (class Index)
import Data.Maybe (Maybe, maybe, maybe')
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object)
import Foreign.Object as Obj

newtype ObjectMap k v = ObjectMap (Object (Tuple k v))

fromArray :: forall k v. EncodeJson k => Array (Tuple k v) -> ObjectMap k v 
fromArray xs = xs
  <#> (\kv -> toJsonStr (fst kv) /\ kv)
  # Obj.fromFoldable
  # ObjectMap

toArray :: forall k v. ObjectMap k v -> Array (Tuple k v) 
toArray (ObjectMap obj) = Obj.values obj

empty :: forall k v. ObjectMap k v
empty = ObjectMap $ Obj.empty

insert :: forall k v. EncodeJson k => k -> v -> ObjectMap k v -> ObjectMap k v
insert key val (ObjectMap obj) = ObjectMap $
  Obj.insert (toJsonStr key) (Tuple key val) obj

delete :: forall k v. EncodeJson k => k -> ObjectMap k v -> ObjectMap k v
delete key (ObjectMap obj) = ObjectMap $ Obj.delete (toJsonStr key) obj

lookup :: forall k v. EncodeJson k => k -> ObjectMap k v -> Maybe v
lookup key (ObjectMap obj) = Obj.lookup (toJsonStr key) obj <#> snd

instance EncodeJson k => Index (ObjectMap k v) k v where
  ix k = affineTraversal set pre
    where
    set :: ObjectMap k v -> v -> ObjectMap k v
    set s b = insert k b s

    pre :: ObjectMap k v -> Either (ObjectMap k v) v
    pre s = maybe (Left s) Right $ lookup k s

instance EncodeJson k => At (ObjectMap k v) k v where
  at k =
    lens (lookup k) \m ->
      maybe' (\_ -> delete k m) \v -> insert k v m

toJsonStr :: forall a. EncodeJson a => a -> String
toJsonStr = encodeJson >>> stringify