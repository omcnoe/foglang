module Main (main) where

import Foglang (hello)
import Test.Hspec (describe, hspec, it)

main :: IO ()
main = hspec $ do
  describe "hello" $ do
    it "prints \"Hello\"" $ do
      hello
