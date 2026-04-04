module Foglang.Test.ProgramsSpec (spec) where

import Data.FileEmbed (embedStringFile)
import Data.Text qualified as T
import Data.Void (Void)
import Foglang.Codegen (genGoFile)
import Foglang.Parser (runParse)
import Foglang.Parser.FogFile (fogFile)
import Foglang.Test.Util (shouldParseAndCodegenTo)
import Test.Hspec (Spec, describe, it)
import Text.Megaparsec (ParseErrorBundle, eof)

import Foglang.AST (FogFile)

runFogParser :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) FogFile
runFogParser path src = runParse (fogFile <* eof) path src

fibonacciFogSrc :: T.Text
fibonacciFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/fibonacci/fibonacci.fog")

fibonacciGoSrc :: T.Text
fibonacciGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/fibonacci/fibonacci.go")

helloWorldFogSrc :: T.Text
helloWorldFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/helloworld/helloworld.fog")

helloWorldGoSrc :: T.Text
helloWorldGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/helloworld/helloworld.go")

indentationFogSrc :: T.Text
indentationFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/indentation/indentation.fog")

indentationGoSrc :: T.Text
indentationGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/indentation/indentation.go")

lambdaFogSrc :: T.Text
lambdaFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/lambda/lambda.fog")

lambdaGoSrc :: T.Text
lambdaGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/lambda/lambda.go")

leapYearFogSrc :: T.Text
leapYearFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/leapyear/leapyear.fog")

leapYearGoSrc :: T.Text
leapYearGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/leapyear/leapyear.go")

nestedIfFogSrc :: T.Text
nestedIfFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/nestedif/nestedif.fog")

nestedIfGoSrc :: T.Text
nestedIfGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/nestedif/nestedif.go")

newtonFogSrc :: T.Text
newtonFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/newton/newton.fog")

newtonGoSrc :: T.Text
newtonGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/newton/newton.go")

packageLevelFogSrc :: T.Text
packageLevelFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/packagelevel/packagelevel.fog")

packageLevelGoSrc :: T.Text
packageLevelGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/packagelevel/packagelevel.go")

partialApplicationFogSrc :: T.Text
partialApplicationFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/partialapplication/partialapplication.fog")

partialApplicationGoSrc :: T.Text
partialApplicationGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/partialapplication/partialapplication.go")

stmtExprFogSrc :: T.Text
stmtExprFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/stmtexpr/stmtexpr.fog")

stmtExprGoSrc :: T.Text
stmtExprGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/stmtexpr/stmtexpr.go")

variadicFogSrc :: T.Text
variadicFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/variadic/variadic.fog")

variadicGoSrc :: T.Text
variadicGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/variadic/variadic.go")

unitFogSrc :: T.Text
unitFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/unit/unit.fog")

unitGoSrc :: T.Text
unitGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/unit/unit.go")

slicesFogSrc :: T.Text
slicesFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/slices/slices.fog")

slicesGoSrc :: T.Text
slicesGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/slices/slices.go")

patternsFogSrc :: T.Text
patternsFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/patterns/patterns.fog")

patternsGoSrc :: T.Text
patternsGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/patterns/patterns.go")

primesFogSrc :: T.Text
primesFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/primes/primes.fog")

primesGoSrc :: T.Text
primesGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/primes/primes.go")

typesFogSrc :: T.Text
typesFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/types/types.fog")

typesGoSrc :: T.Text
typesGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/types/types.go")

inferenceFogSrc :: T.Text
inferenceFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/inference/inference.fog")

inferenceGoSrc :: T.Text
inferenceGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/inference/inference.go")

primesInferenceFogSrc :: T.Text
primesInferenceFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/primes-inference/primes-inference.fog")

primesInferenceGoSrc :: T.Text
primesInferenceGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Programs/primes-inference/primes-inference.go")

spec :: Spec
spec = describe "programs" $ do
  it "fibonacci.fog -> fibonacci.go" $
    fmap genGoFile (runFogParser "fibonacci.fog" fibonacciFogSrc)
      `shouldParseAndCodegenTo` fibonacciGoSrc
  it "helloworld.fog -> helloworld.go" $
    fmap genGoFile (runFogParser "helloworld.fog" helloWorldFogSrc)
      `shouldParseAndCodegenTo` helloWorldGoSrc
  it "lambda.fog -> lambda.go" $
    fmap genGoFile (runFogParser "lambda.fog" lambdaFogSrc)
      `shouldParseAndCodegenTo` lambdaGoSrc
  it "leapyear.fog -> leapyear.go" $
    fmap genGoFile (runFogParser "leapyear.fog" leapYearFogSrc)
      `shouldParseAndCodegenTo` leapYearGoSrc
  it "indentation.fog -> indentation.go" $
    fmap genGoFile (runFogParser "indentation.fog" indentationFogSrc)
      `shouldParseAndCodegenTo` indentationGoSrc
  it "nestedif.fog -> nestedif.go" $
    fmap genGoFile (runFogParser "nestedif.fog" nestedIfFogSrc)
      `shouldParseAndCodegenTo` nestedIfGoSrc
  it "newton.fog -> newton.go" $
    fmap genGoFile (runFogParser "newton.fog" newtonFogSrc)
      `shouldParseAndCodegenTo` newtonGoSrc
  it "packagelevel.fog -> packagelevel.go" $
    fmap genGoFile (runFogParser "packagelevel.fog" packageLevelFogSrc)
      `shouldParseAndCodegenTo` packageLevelGoSrc
  it "partialapplication.fog -> partialapplication.go" $
    fmap genGoFile (runFogParser "partialapplication.fog" partialApplicationFogSrc)
      `shouldParseAndCodegenTo` partialApplicationGoSrc
  it "stmtexpr.fog -> stmtexpr.go" $
    fmap genGoFile (runFogParser "stmtexpr.fog" stmtExprFogSrc)
      `shouldParseAndCodegenTo` stmtExprGoSrc
  it "variadic.fog -> variadic.go" $
    fmap genGoFile (runFogParser "variadic.fog" variadicFogSrc)
      `shouldParseAndCodegenTo` variadicGoSrc
  it "unit.fog -> unit.go" $
    fmap genGoFile (runFogParser "unit.fog" unitFogSrc)
      `shouldParseAndCodegenTo` unitGoSrc
  it "slices.fog -> slices.go" $
    fmap genGoFile (runFogParser "slices.fog" slicesFogSrc)
      `shouldParseAndCodegenTo` slicesGoSrc
  it "patterns.fog -> patterns.go" $
    fmap genGoFile (runFogParser "patterns.fog" patternsFogSrc)
      `shouldParseAndCodegenTo` patternsGoSrc
  it "primes.fog -> primes.go" $
    fmap genGoFile (runFogParser "primes.fog" primesFogSrc)
      `shouldParseAndCodegenTo` primesGoSrc
  it "types.fog -> types.go" $
    fmap genGoFile (runFogParser "types.fog" typesFogSrc)
      `shouldParseAndCodegenTo` typesGoSrc
  it "inference.fog -> inference.go" $
    fmap genGoFile (runFogParser "inference.fog" inferenceFogSrc)
      `shouldParseAndCodegenTo` inferenceGoSrc
  it "primes-inference.fog -> primes-inference.go" $
    fmap genGoFile (runFogParser "primes-inference.fog" primesInferenceFogSrc)
      `shouldParseAndCodegenTo` primesInferenceGoSrc
