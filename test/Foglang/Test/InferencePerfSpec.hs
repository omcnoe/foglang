-- | Regression tests against the O(N^2) pathology the union-find inference
-- refactor is designed to eliminate. Pre-refactor, N=256 emitted ~97k errors
-- in ~6s; N=1024 did not complete within 30s. Post-refactor, both complete
-- in under a second with exactly one error.
module Foglang.Test.InferencePerfSpec (spec) where

import Control.Exception (evaluate)
import Data.Text qualified as T
import System.Timeout (timeout)
import Test.Hspec (Spec, describe, it, shouldBe, expectationFailure)
import Text.Megaparsec (eof)
import Foglang.Inference (InferError (..), inferAndResolve)
import Foglang.Parser (SC(..), runParse, scn)
import Foglang.Parser.Expr (childBlockExprSequence)

-- | Build `let f a1 ... aN = a1[0] + a2[a1] + ... + aN[a(N-1)]`.
--
-- The `+` chain is load-bearing. It forces every value type to unify
-- numerically, which pins each `a_i`'s type to `TSlice int`, which then
-- appears as the KEY of `a_(i+1)`. Inference must build the full N-deep
-- Link chain in the substitution before any of it can resolve; pre-refactor,
-- each applySubst chases the whole chain, giving O(N^2) overall.
pathologicalIndexableChain :: Int -> T.Text
pathologicalIndexableChain n =
  T.concat
    [ "let f"
    , T.concat [" a" <> T.pack (show i) | i <- [1..n]]
    , " =\n  a1[0]"
    , T.concat [" + a" <> T.pack (show i) <> "[a" <> T.pack (show (i-1)) <> "]" | i <- [2..n]]
    ]

-- | Run inference with a wall-clock budget in milliseconds. `Nothing` on timeout.
--
-- `evaluate` forces WHNF of the Either inside the timeout; without it the
-- result would be a lazy thunk that timeout can return immediately, and the
-- actual inference work would happen later outside the timeout.
inferWithinMs :: Int -> T.Text -> IO (Maybe (Either [InferError] ()))
inferWithinMs millis src =
  timeout (millis * 1000) $
    case runParse (childBlockExprSequence <* runSC scn <* eof) "perf-test" src of
      Left err -> error ("parse failed: " <> show err)
      Right (expr, pstate) ->
        -- Discard the resolved tree; only the Either shape and error
        -- count matter here.
        evaluate $ case inferAndResolve (expr, pstate) of
          Left errs -> Left errs
          Right _   -> Right ()

spec :: Spec
spec = describe "inference performance" $ do
  describe "pathological indexable addition chain" $ do
    -- N=256; post-refactor: sub-second; pre-refactor: ~6s with ~97k errors.
    it "256-level chain completes within budget" $ do
      mResult <- inferWithinMs 5000 (pathologicalIndexableChain 256)
      case mResult of
        Nothing -> expectationFailure "inference exceeded 5000ms budget"
        Just _  -> pure ()

    -- N=1024; post-refactor: sub-second; Pre-refactor: >30s (timeout).
    it "1024-level chain completes within budget" $ do
      mResult <- inferWithinMs 10000 (pathologicalIndexableChain 1024)
      case mResult of
        Nothing -> expectationFailure "inference exceeded 10000ms budget"
        Just _  -> pure ()

    -- Folded resolve short-circuits Either on the first CannotInferType.
    it "1024-level chain reports exactly one error" $ do
      mResult <- inferWithinMs 10000 (pathologicalIndexableChain 1024)
      case mResult of
        Nothing -> expectationFailure "inference exceeded 10000ms budget"
        Just (Right _) -> expectationFailure "expected CannotInferType errors, got clean resolve"
        Just (Left errs) -> length errs `shouldBe` 1
