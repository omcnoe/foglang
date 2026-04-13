-- | Performance regression tests for type inference.
--
-- These tests guard against the O(N²) failure mode we had with the
-- map-based substitution on deeply-nested indexable chains. Under the
-- union-find refactor, each such chain runs in near-linear time.
--
-- The inputs here are pathological by construction (no real fog program
-- would look like this). They're stress tests for the inference +
-- resolution pipeline, not integration tests for user-facing behaviour.
module Foglang.Test.InferencePerfSpec (spec) where

import Data.Text qualified as T
import Foglang.Inference (InferError (..), inferAndResolve)
import Foglang.Parser (SC(..), runParse, scn)
import Foglang.Parser.Expr (childBlockExprSequence)
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, expectationFailure)
import Text.Megaparsec (eof)

-- | Build a source string of the form
--   `let f a1 a2 ... aN = a1[0] + a2[a1] + a3[a2] + ... + aN[a(N-1)]`
-- Each `ai` ends up with an indexable type whose key constraint is
-- `a(i-1)`'s type — an N-deep dependency chain through the substitution.
pathologicalIndexableChain :: Int -> T.Text
pathologicalIndexableChain n =
  T.concat
    [ "let f"
    , T.concat [" a" <> T.pack (show i) | i <- [1..n]]
    , " =\n  a1[0]"
    , T.concat [" + a" <> T.pack (show i) <> "[a" <> T.pack (show (i-1)) <> "]" | i <- [2..n]]
    ]

-- | Run inference on a source string with a wall-clock budget. Returns
-- `Nothing` on timeout, otherwise the inference result.
inferWithin :: Int -> T.Text -> IO (Maybe (Either [InferError] ()))
inferWithin microseconds src =
  timeout microseconds $ do
    case runParse (childBlockExprSequence <* runSC scn <* eof) "perf-test" src of
      Left err -> error ("parse failed: " <> show err)
      Right (expr, pstate) ->
        -- Discard the resolved Expr — we only care about the Either shape
        -- and the number of errors, not the tree contents.
        let result = inferAndResolve (expr, pstate)
        in pure $ case result of
             Left errs -> Left errs
             Right _   -> Right ()

spec :: Spec
spec = describe "inference performance" $ do
  describe "pathological indexable chain" $ do
    -- N=256 under union-find completes in well under a second on any
    -- reasonable machine. Quadratic behaviour would take minutes.
    it "256-level chain completes within budget" $ do
      mResult <- inferWithin (5 * 1000 * 1000) (pathologicalIndexableChain 256)
      case mResult of
        Nothing -> expectationFailure "inference exceeded 5-second budget"
        Just _  -> pure ()

    -- N=1024 is ~4x larger input and would be ~16x slower under O(N²).
    -- Linear (or near-linear) behaviour keeps the wall-clock manageable.
    it "1024-level chain completes within budget" $ do
      mResult <- inferWithin (10 * 1000 * 1000) (pathologicalIndexableChain 1024)
      case mResult of
        Nothing -> expectationFailure "inference exceeded 10-second budget"
        Just _  -> pure ()

    -- Also verify the expected error shape. Under the old implementation
    -- the N-deep chain emitted O(N²) CannotInferType errors (one per
    -- surviving TypeVarIndexable nested inside another). Under folded
    -- resolve the Either monad short-circuits at the first indexable-key
    -- barrier, so exactly one error is produced — any regression that
    -- stops short-circuiting (e.g. collecting errors applicatively) or
    -- reintroduces duplicate reporting shows up as a jump here.
    it "1024-level chain reports exactly one error" $ do
      mResult <- inferWithin (10 * 1000 * 1000) (pathologicalIndexableChain 1024)
      case mResult of
        Nothing -> expectationFailure "inference exceeded 10-second budget"
        Just (Left errs) -> length errs `shouldBe` 1
        Just (Right _) ->
          expectationFailure "expected CannotInferType errors, got clean resolve"
