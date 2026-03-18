module Main (main) where

import Data.Text.IO qualified as TIO
import Foglang.Codegen (codegenGoFile)
import Foglang.Parser.FogFile (fogFile)
import System.Environment (getArgs)
import Text.Megaparsec (eof, parse)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      src <- TIO.readFile path
      case parse (fogFile <* eof) path src of
        Left err -> fail (show err)
        Right ff -> codegenGoFile ff >>= TIO.putStr
    _ -> putStrLn "usage: fog <file.fog>"
