module Foglang.Test.Parser.HeaderSpec (spec) where

import Control.Monad.State.Strict (evalState)
import Data.Either (isLeft)
import Foglang.AST (Header (..), ImportAlias (..), ImportDecl (..), PackageClause (..))
import Foglang.Parser.Header (header)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof, runParserT)

spec :: Spec
spec = do
  let parseHeader s = evalState (runParserT (header <* eof) "HeaderSpec.hs" s) 0

  describe "header parses" $ do
    it "single imports, grouped imports, all alias kinds, and empty groups" $
      parseHeader
        "package main\n\
        \import \"fmt\"\n\
        \import (\n\t\"os\"\n\t\"strconv\"\n)\n\
        \import (. \"math/rand\" _ \"math/big\" myalias \"math/complex\")\n\
        \import ()\n\
        \import \"math\""
        `shouldBe` Right
          ( Header
              (PackageClause "main")
              [ ImportDecl Default "fmt"
              , ImportDecl Default "os"
              , ImportDecl Default "strconv"
              , ImportDecl Dot "math/rand"
              , ImportDecl Blank "math/big"
              , ImportDecl (Alias "myalias") "math/complex"
              , ImportDecl Default "math"
              ]
          )

  describe "header rejects" $ do
    it "missing package declaration" $
      parseHeader "import \"fmt\"" `shouldSatisfy` isLeft
