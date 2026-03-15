module Foglang.Test.Util (shouldParseAndCodegenTo) where

import Data.Text qualified as T
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import Test.Hspec (expectationFailure)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

-- | Like `shouldBe` for parse+codegen results, but prints raw text (with real
-- newlines/tabs) as well as escaped string literals on failure.
shouldParseAndCodegenTo ::
  (HasCallStack) =>
  Either (ParseErrorBundle T.Text Void) T.Text ->
  T.Text ->
  IO ()
shouldParseAndCodegenTo (Left err) _ =
  expectationFailure $ "parse error:\n" <> errorBundlePretty err
shouldParseAndCodegenTo (Right actual) expected
  | actual == expected = pure ()
  | otherwise =
      expectationFailure $
        "expected:\n"
          <> indent (T.unpack expected)
          <> "\n"
          <> "but got:\n"
          <> indent (T.unpack actual)
          <> "\n"
          <> "expected (escaped): "
          <> show expected
          <> "\n"
          <> "but got (escaped): "
          <> show actual
          <> "\n"
  where
    indent = unlines . map ("    " <>) . lines
