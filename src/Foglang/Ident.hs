module Foglang.Ident (Ident, ident) where

import Data.Char (isDigit)
import qualified Data.Text as T
import Foglang.Core (Parser, isLetter)
import Text.Megaparsec (satisfy, takeWhileP)

-- identifier = letter { letter | unicode_digit } .

type Ident = T.Text

ident :: Parser Ident
ident = do
  c <- satisfy isLetter
  cs <- takeWhileP Nothing (\ch -> isLetter ch || isDigit ch)
  return $ T.cons c cs
