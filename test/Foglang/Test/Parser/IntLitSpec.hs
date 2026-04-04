module Foglang.Test.Parser.IntLitSpec (spec) where

import Data.Either (isLeft)
import Foglang.AST (IntLit (..))
import Foglang.Parser (runParse)
import Foglang.Parser.IntLit (intLit)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof)

spec :: Spec
spec = do
  let specValid =
        [ ("42", IntDecimal),
          ("4_2", IntDecimal),
          ("0600", IntOctal),
          ("0_600", IntOctal),
          ("0o600", IntOctal),
          ("0O600", IntOctal),
          ("0xBadFace", IntHex),
          ("0xBad_Face", IntHex),
          ("0x_67_7a_2f_cc_40_c6", IntHex),
          ("170141183460469231731687303715884105727", IntDecimal),
          ("170_141183_460469_231731_687303_715884_105727", IntDecimal)
        ]

  let extraValid =
        [ ("0", IntDecimal),
          ("1", IntDecimal),
          ("0b1010", IntBinary),
          ("0B0000", IntBinary),
          ("0b_1010", IntBinary),
          ("0o_600", IntOctal)
        ]

  let specInvalid =
        [ "_42", --       an identifier, not an integer literal
          "42_", --       invalid: _ must separate successive digits
          "4__2", --      invalid: only one _ at a time
          "0_xBadFace" -- invalid: _ must separate successive digits
        ]

  let doubleUnderscoreAfterPrefixInvalid =
        [ -- invalid: digitSeq must reject leading '_'
          "0b__1",
          "0O__7",
          "0X__0"
        ]

  let parseIntLit s = runParse (intLit <* eof) "IntLitSpec.hs" s

  describe "intLit parses" $ do
    it "go spec examples" $
      mapM_ (\(s, con) -> parseIntLit s `shouldBe` Right (con s)) specValid
    it "extra examples" $
      mapM_ (\(s, con) -> parseIntLit s `shouldBe` Right (con s)) extraValid

  describe "intLit rejects" $ do
    it "go spec invalid examples" $
      mapM_ (\s -> parseIntLit s `shouldSatisfy` isLeft) specInvalid
    it "double underscore after prefix" $
      mapM_ (\s -> parseIntLit s `shouldSatisfy` isLeft) doubleUnderscoreAfterPrefixInvalid
