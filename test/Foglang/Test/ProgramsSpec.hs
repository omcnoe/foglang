module Foglang.Test.ProgramsSpec (spec) where

import Data.FileEmbed (embedStringFile)
import Data.Text qualified as T
import Foglang.Codegen (codegenGoFile)
import Foglang.Parser.FogFile (fogFile)
import Foglang.Test.Util (shouldParseAndCodegenTo)
import Test.Hspec (Spec, describe, it)
import Text.Megaparsec (eof, parse)

fibonacciFogSrc :: T.Text
fibonacciFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/fibonacci.fog")

fibonacciGoSrc :: T.Text
fibonacciGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/fibonacci.go")

helloWorldFogSrc :: T.Text
helloWorldFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/helloworld.fog")

helloWorldGoSrc :: T.Text
helloWorldGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/helloworld.go")

lambdaFogSrc :: T.Text
lambdaFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/lambda.fog")

lambdaGoSrc :: T.Text
lambdaGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/lambda.go")

leapYearFogSrc :: T.Text
leapYearFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/leapyear.fog")

leapYearGoSrc :: T.Text
leapYearGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/leapyear.go")

nestedIfFogSrc :: T.Text
nestedIfFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/nestedif.fog")

nestedIfGoSrc :: T.Text
nestedIfGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/nestedif.go")

packageLevelFogSrc :: T.Text
packageLevelFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/packagelevel.fog")

packageLevelGoSrc :: T.Text
packageLevelGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/packagelevel.go")

spec :: Spec
spec = describe "programs" $ do
  it "fibonacci.fog -> fibonacci.go" $
    fmap codegenGoFile (parse (fogFile <* eof) "fibonacci.fog" fibonacciFogSrc)
      `shouldParseAndCodegenTo` fibonacciGoSrc
  it "helloworld.fog -> helloworld.go" $
    fmap codegenGoFile (parse (fogFile <* eof) "helloworld.fog" helloWorldFogSrc)
      `shouldParseAndCodegenTo` helloWorldGoSrc
  it "lambda.fog -> lambda.go" $
    fmap codegenGoFile (parse (fogFile <* eof) "lambda.fog" lambdaFogSrc)
      `shouldParseAndCodegenTo` lambdaGoSrc
  it "leapyear.fog -> leapyear.go" $
    fmap codegenGoFile (parse (fogFile <* eof) "leapyear.fog" leapYearFogSrc)
      `shouldParseAndCodegenTo` leapYearGoSrc
  it "nestedif.fog -> nestedif.go" $
    fmap codegenGoFile (parse (fogFile <* eof) "nestedif.fog" nestedIfFogSrc)
      `shouldParseAndCodegenTo` nestedIfGoSrc
  it "packagelevel.fog -> packagelevel.go" $
    fmap codegenGoFile (parse (fogFile <* eof) "packagelevel.fog" packageLevelFogSrc)
      `shouldParseAndCodegenTo` packageLevelGoSrc
