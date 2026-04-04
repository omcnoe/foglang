module Main (main) where

import Data.Text.IO qualified as TIO
import Foglang.Codegen (genGoFile)
import Foglang.Parser (runParse)
import Foglang.Parser.FogFile (fogFile)
import System.Environment (getArgs)
import Text.Megaparsec (eof)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      src <- TIO.readFile path
      case runParse (fogFile <* eof) path src of
        Left err -> fail (show err)
        Right ff -> genGoFile ff >>= TIO.putStr
    _ -> putStrLn "usage: fog <file.fog>"
