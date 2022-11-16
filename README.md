# purescript-object-maps

An `ObjectMap k v` represents maps from keys of type k to values of type v. It _keeps its insertion order_ and has efficient lookups. The insertion performance however is O(n), equal to the underlying [Foreign.Object#Object](https://pursuit.purescript.org/packages/purescript-foreign-object).

Keys need an instance of [Data.Argonaut.Encode.Class#EncodeJson](https://pursuit.purescript.org/packages/purescript-argonaut-codecs/docs/Data.Argonaut.Encode.Class#t:EncodeJson) for most operations.