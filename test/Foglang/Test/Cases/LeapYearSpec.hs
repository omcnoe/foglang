module Foglang.Test.Cases.LeapYearSpec (spec) where

import Data.FileEmbed (embedStringFile)
import Data.Text qualified as T
import Foglang.Codegen (codegenGoFile)
import Foglang.Parser.FogFile (fogFile)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (eof, parse)

leapYearFogSrc :: T.Text
leapYearFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Cases/leapyear.fog")

leapYearGoSrc :: T.Text
leapYearGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Cases/leapyear.go")

spec :: Spec
spec = describe "leapyear" $ do
  it "parses and codegens to expected Go output" $
    fmap codegenGoFile (parse (fogFile <* eof) "leapyear.fog" leapYearFogSrc)
      `shouldBe` Right leapYearGoSrc
