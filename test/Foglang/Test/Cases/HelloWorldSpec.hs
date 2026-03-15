module Foglang.Test.Cases.HelloWorldSpec (spec) where

import Data.FileEmbed (embedStringFile)
import Data.Text qualified as T
import Foglang.Codegen (codegenGoFile)
import Foglang.Parser.FogFile (fogFile)
import Foglang.Test.Util (shouldParseAndCodegenTo)
import Test.Hspec (Spec, describe, it)
import Text.Megaparsec (eof, parse)

helloWorldFogSrc :: T.Text
helloWorldFogSrc = T.pack $(embedStringFile "test/Foglang/Test/Cases/helloworld.fog")

helloWorldGoSrc :: T.Text
helloWorldGoSrc = T.pack $(embedStringFile "test/Foglang/Test/Cases/helloworld.go")

spec :: Spec
spec = describe "helloworld" $ do
  it "parses and codegens to expected Go output" $
    fmap codegenGoFile (parse (fogFile <* eof) "helloworld.fog" helloWorldFogSrc)
      `shouldParseAndCodegenTo` helloWorldGoSrc
