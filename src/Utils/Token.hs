module Utils.Token where

data Token =
  -- values
  Int Integer
  | Bool Bool
  | Var String

  -- symbols
  | Semicolon -- ';'
  | LParen -- '('
  | RParen -- ')'

  -- operators
  | Assign -- ':='
  | Not -- '~'
  | Plus -- '+'
  | Minus -- '-'
  | Times -- '*'
  | Eq -- '='
  | EqEq -- '=='

  -- keywords
  | If -- 'if'
  | Then -- 'then'
  | Else -- 'else'
  | While -- 'while'
  | Until -- 'until'
  | Do -- 'do'

  deriving Show
