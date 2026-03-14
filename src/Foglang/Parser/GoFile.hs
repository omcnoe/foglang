module Foglang.Parser.GoFile (goFile) where

import Foglang.AST (GoFile (..))
import Foglang.Parser (Parser)
import Foglang.Parser.Expr (expr)
import Foglang.Parser.Header (header)
import Text.Megaparsec (many)

-- TODO needs refactoring when we work out proper package/module etc. design
goFile :: Parser GoFile
goFile = GoFile <$> header <*> many expr
