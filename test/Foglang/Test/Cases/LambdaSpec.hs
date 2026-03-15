module Foglang.Test.Cases.LambdaSpec (spec) where

import Data.FileEmbed (embedStringFile)
import Data.Text qualified as T
import Foglang.Codegen (codegenGoFile)
import Foglang.Parser.FogFile (fogFile)
import Foglang.Test.Util (shouldParseAndCodegenTo)
import Test.Hspec (Spec, describe, it)
import Text.Megaparsec (eof, parse)

lambdaFogSrc :: T.Text
lambdaFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Cases/lambda.fog")

lambdaGoSrc :: T.Text
lambdaGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Cases/lambda.go")

spec :: Spec
spec = describe "lambda" $ do
  it "parses and codegens to expected Go output" $
    fmap codegenGoFile (parse (fogFile <* eof) "lambda.fog" lambdaFogSrc)
      `shouldParseAndCodegenTo` lambdaGoSrc
