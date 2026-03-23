module Main (main) where

import Control.Monad.State.Strict (evalState)
import Data.Text.IO qualified as TIO
import Foglang.Codegen (codegenGoFile)
import Foglang.Parser.FogFile (fogFile)
import System.Environment (getArgs)
import Text.Megaparsec (eof, runParserT)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      src <- TIO.readFile path
      case evalState (runParserT (fogFile <* eof) path src) 0 of
        Left err -> fail (show err)
        Right ff -> codegenGoFile ff >>= TIO.putStr
    _ -> putStrLn "usage: fog <file.fog>"
