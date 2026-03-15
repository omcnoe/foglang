module Foglang.Test.Parser.HeaderSpec (spec) where

import Data.Either (isLeft)
import Foglang.AST (Header (..), ImportAlias (..), ImportDecl (..), PackageClause (..))
import Foglang.Parser.Header (header)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof, parse)

spec :: Spec
spec = do
  let parseHeader s = parse (header <* eof) "HeaderSpec.hs" s

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
              [ ImportDecl None "fmt"
              , ImportDecl None "os"
              , ImportDecl None "strconv"
              , ImportDecl Dot "math/rand"
              , ImportDecl Blank "math/big"
              , ImportDecl (Alias "myalias") "math/complex"
              , ImportDecl None "math"
              ]
          )

  describe "header rejects" $ do
    it "missing package declaration" $
      parseHeader "import \"fmt\"" `shouldSatisfy` isLeft
