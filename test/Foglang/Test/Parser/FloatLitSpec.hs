module Foglang.Test.Parser.FloatLitSpec (spec) where

import Data.Either (isLeft)
import Foglang.AST (FloatLit (..))
import Foglang.Parser (runParse)
import Foglang.Parser.FloatLit (floatLit)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (eof)

spec :: Spec
spec = do
  let specValid =
        [ ("0.", FloatDecimal),
          ("72.40", FloatDecimal),
          ("072.40", FloatDecimal), --    == 72.40
          ("2.71828", FloatDecimal),
          ("1.e+0", FloatDecimal),
          ("6.67428e-11", FloatDecimal),
          ("1E6", FloatDecimal),
          (".25", FloatDecimal),
          (".12345E+5", FloatDecimal),
          ("1_5.", FloatDecimal), --      == 15.0
          ("0.15e+0_2", FloatDecimal), -- == 15.0
          ("0x1p-2", FloatHex), --        == 0.25
          ("0x2.p10", FloatHex), --       == 2048.0
          ("0x1.Fp+0", FloatHex), --      == 1.9375
          ("0X.8p-0", FloatHex), --       == 0.5
          ("0X_1FFFP-16", FloatHex) --    == 0.1249847412109375
        ]

  let specInvalid =
        [ "0x15e-2", --  0x15e - 2 (integer subtraction)
          "0x.p1", --    invalid: mantissa has no digits
          "1p-2", --     invalid: p exponent requires hexadecimal mantissa
          "0x1.5e-2", -- invalid: hexadecimal mantissa requires p exponent
          "1_.5", --     invalid: _ must separate successive digits
          "1._5", --     invalid: _ must separate successive digits
          "1.5_e1", --   invalid: _ must separate successive digits
          "1.5e_1", --   invalid: _ must separate successive digits
          "1.5e1_" --    invalid: _ must separate successive digits
        ]

  let parseFloatLit s = runParse (floatLit <* eof) "FloatLitSpec.hs" s

  describe "floatLit parses" $ do
    it "go spec examples" $
      mapM_ (\(s, con) -> parseFloatLit s `shouldBe` Right (con s)) specValid

  describe "floatLit rejects" $
    it "go spec invalid examples" $
      mapM_ (\s -> parseFloatLit s `shouldSatisfy` isLeft) specInvalid
