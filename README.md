# purescript-object-maps

Map-like data structure with stable insertion order

## Description

An `ObjectMap k v` represents maps from keys of type k to values of type v. It _keeps its insertion order_ and has efficient lookups. The insertion performance however is O(n), equal to the underlying [Object](https://pursuit.purescript.org/packages/purescript-foreign-object).

Keys need an instance of [EncodeJson](https://pursuit.purescript.org/packages/purescript-argonaut-codecs/docs/Data.Argonaut.Encode.Class#t:EncodeJson) for most operations.

## Example

Consider the following sample object:

```hs
sample :: ObjectMap Int String
sample = OM.empty
  # OM.insert 2 "2"
  # OM.insert 3 "3"
  # OM.insert 1 "1"
```

The REPL output shows that the order is preserved.
```
> toArray sample  
[(Tuple 2 "2"),(Tuple 3 "3"),(Tuple 1 "1")]
```

As previously said, `insert` has O(n) time complexity. If you want better performance, you can use the function
`poke` of the module `Data.ObjectMap.ST` which has O(1) average time complexity.

```hs
import Control.Monad.ST as ST
import Data.ObjectMap (ObjectMap)
import Data.ObjectMap.ST as STOM

sample :: ObjectMap Int String
sample = ST.run (do
          m <- STOM.new
          for_ (1..10000) \n -> do
            void $ STOM.poke i (show i) m
          STOM.unsafeFreeze m
)
```

## FAQ

### Isn't it unsafe to rely on JavaScripts Object property order?

Since ES2015 JavaScript object properties are guaranteed to preserve insertion order for string keys. This library ensures that every property is interpreted as string by the JavaScript engine. Thus the order can be relied on.


### Why not using native ES5 Maps which allow arbitrary JavaScript values for keys?
Lookups in native ES5 Maps would rely on JavaScript's equality model which is very different from PureScript's.


### Why do the library functions only need an `EncodeJson` instance and never need a `DecodeJson`?
Keys are stored twice, in encoded and in raw format. Thus there is no need to decode them back. This was done to avoid relying on roundtrip implementations for the two instances.


### Why `EncodeJson` anyway and not e.g. [Hashable](https://pursuit.purescript.org/packages/purescript-unordered-collections/3.0.1/docs/Data.Hashable#t:Hashable)?
  This would have been possible too. However, `EncodeJson` was chosen because it was considered to be more likely that those instances already exist. This is opinionated, though.


## Contribution

It's planned to port more useful functions from `Foreign.Object` and `Data.Map`. Any PR's are welcome.
