module Foglang.Parser.FogFile (fogFile) where

import Foglang.AST (FogFile (..))
import Foglang.Parser (Parser, scn)
import Foglang.Parser.Expr (sequence')
import Foglang.Parser.Header (header)
import Text.Megaparsec (getSourcePos)
import Text.Megaparsec.Pos (sourceLine)

fogFile :: Parser FogFile
fogFile = do
  topLine <- sourceLine <$> getSourcePos
  h <- header
  body <- sequence' Nothing topLine <* scn
  return $ FogFile h body
