module Foglang.Test.Parser.HeaderSpec (spec) where

import Data.Either (isLeft)
import Foglang.AST (Header (..), ImportAlias (..), ImportDecl (..), PackageClause (..))
import Foglang.Parser.Header (header)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof, parse)

spec :: Spec
spec = do
  let parseHeader s = parse (header <* eof) "HeaderSpec.hs" s

  describe "header" $ do
    it "parses package declaration with no imports" $
      parseHeader "package main" `shouldBe` Right (Header (PackageClause "main") [])

    it "parses package with a single import" $
      parseHeader
        "package main\n\
        \import \"fmt\""
        `shouldBe` Right (Header (PackageClause "main") [ImportDecl None "fmt"])

    it "parses package with multiple single imports" $
      parseHeader
        "package main\n\
        \import \"fmt\"\n\
        \import \"os\""
        `shouldBe` Right
          ( Header
              (PackageClause "main")
              [ImportDecl None "fmt", ImportDecl None "os"]
          )

    it "parses package with a grouped import" $
      parseHeader
        "package main\n\
        \import (\"fmt\" \"os\")"
        `shouldBe` Right
          ( Header
              (PackageClause "main")
              [ImportDecl None "fmt", ImportDecl None "os"]
          )

    it "parses package with mixed single and grouped imports" $
      parseHeader
        "package main\n\
        \import \"fmt\"\n\
        \import (\"os\" \"strconv\")"
        `shouldBe` Right
          ( Header
              (PackageClause "main")
              [ImportDecl None "fmt", ImportDecl None "os", ImportDecl None "strconv"]
          )

    it "parses grouped import with import paths on separate lines" $
      parseHeader
        "package main\n\
        \import (\n\
        \  \"fmt\"\n\
        \  \"os\"\n\
        \)"
        `shouldBe` Right
          ( Header
              (PackageClause "main")
              [ImportDecl None "fmt", ImportDecl None "os"]
          )

    it "parses package with an empty grouped import" $
      parseHeader
        "package main\n\
        \import ()"
        `shouldBe` Right (Header (PackageClause "main") [])

    it "rejects input missing package declaration" $
      parseHeader "import \"fmt\"" `shouldSatisfy` isLeft

    it "rejects empty input" $
      parseHeader "" `shouldSatisfy` isLeft

    it "parses an unqualified (dot) import" $
      parseHeader
        "package main\n\
        \import . \"fmt\""
        `shouldBe` Right (Header (PackageClause "main") [ImportDecl Dot "fmt"])
