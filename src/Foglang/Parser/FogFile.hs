module Foglang.Parser.FogFile (fogFile) where

import Foglang.AST (FogFile (..))
import Foglang.Parser (Parser, SC(..), scn)
import Foglang.Parser.Expr (sequenceWithNewline, LineIndent(..))
import Foglang.Parser.Header (header)
import Text.Megaparsec (getSourcePos)
import Text.Megaparsec.Pos (sourceLine)

fogFile :: Parser FogFile
fogFile = do
  topLine <- sourceLine <$> getSourcePos
  h <- header
  body <- sequenceWithNewline (LineIndent 0) topLine <* runSC scn
  return $ FogFile h body
