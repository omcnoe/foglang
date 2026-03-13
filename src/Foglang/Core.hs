module Foglang.Core (Parser, isLetter) where

import qualified Data.Char as Hidden.Data.Char (isLetter)
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (Parsec)

type Parser = Parsec Void T.Text

-- letter includes underscore in go, hide the unsafe one from Data.Char
isLetter :: Char -> Bool
isLetter c = Hidden.Data.Char.isLetter c || c == '_'
