module Test.Main where

import Prelude

import Data.ObjectMap (ObjectMap)
import Data.ObjectMap as OM
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Debug (spy)
import Effect (Effect)
import Effect.Aff (launchAff_, delay)
import Test.Spec (pending, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

sample :: ObjectMap Int String
sample = OM.empty
  # OM.insert 2 "2"
  # OM.insert 3 "3"
  # OM.insert 1 "1"

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "insertion" do
    it "keeps order" do
      OM.empty
        # OM.insert "b" 2
        # OM.insert "c" 3
        # OM.insert "a" 1
        # OM.toArray
        # shouldEqual
            [ "b" /\ 2
            , "c" /\ 3
            , "a" /\ 1
            ]
  describe "bulk insertion with numbers" do
    it "keeps order" do
      OM.fromArray
        [ 2 /\ 2
        , 3 /\ 3
        , 1 /\ 1
        ]
        # OM.toArray
        # shouldEqual
            [ 2 /\ 2
            , 3 /\ 3
            , 1 /\ 1
            ]
