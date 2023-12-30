module Utils.Token where

data Token =
  Var String -- variables

  -- values
  | I Integer -- integers
  | B Bool -- booleans

  -- symbols
  | Semicolon -- ';'
  | LParen -- '('
  | RParen -- ')'

  -- operators
  | Assign -- ':='
  | Not -- 'not'
  | Plus -- '+'
  | Minus -- '-'
  | Times -- '*'
  | IEqu -- '='
  | BEqu -- '=='
  | Le -- '<='

  -- keywords
  | If -- 'if'
  | Then -- 'then'
  | Else -- 'else'
  | While -- 'while'
  | Until -- 'until'
  | Do -- 'do'

  deriving Show
