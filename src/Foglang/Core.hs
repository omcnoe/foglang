module Foglang.Core (Parser, isLetter) where

import qualified Data.Char as Data.Char (isLetter)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void T.Text

-- Go's `letter` includes underscore, so we wrap Data.Char.isLetter
isLetter :: Char -> Bool
isLetter c = Data.Char.isLetter c || c == '_'
