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

newtonFogSrc :: T.Text
newtonFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/newton.fog")

newtonGoSrc :: T.Text
newtonGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/newton.go")

packageLevelFogSrc :: T.Text
packageLevelFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/packagelevel.fog")

packageLevelGoSrc :: T.Text
packageLevelGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/packagelevel.go")

partialApplicationFogSrc :: T.Text
partialApplicationFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/partialapplication.fog")

partialApplicationGoSrc :: T.Text
partialApplicationGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/partialapplication.go")

variadicFogSrc :: T.Text
variadicFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/variadic.fog")

variadicGoSrc :: T.Text
variadicGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/variadic.go")

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
  it "newton.fog -> newton.go" $
    fmap codegenGoFile (parse (fogFile <* eof) "newton.fog" newtonFogSrc)
      `shouldParseAndCodegenTo` newtonGoSrc
  it "packagelevel.fog -> packagelevel.go" $
    fmap codegenGoFile (parse (fogFile <* eof) "packagelevel.fog" packageLevelFogSrc)
      `shouldParseAndCodegenTo` packageLevelGoSrc
  it "partialapplication.fog -> partialapplication.go" $
    fmap codegenGoFile (parse (fogFile <* eof) "partialapplication.fog" partialApplicationFogSrc)
      `shouldParseAndCodegenTo` partialApplicationGoSrc
  it "variadic.fog -> variadic.go" $
    fmap codegenGoFile (parse (fogFile <* eof) "variadic.fog" variadicFogSrc)
      `shouldParseAndCodegenTo` variadicGoSrc
