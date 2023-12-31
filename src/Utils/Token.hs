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
  | Add -- '+'
  | Mult -- '*'
  | Sub -- '-'
  | Le -- '<='
  | IEqu -- '=='
  | Not -- 'not'
  | BEqu -- '='
  | And -- 'and'

  -- keywords
  | If -- 'if'
  | Then -- 'then'
  | Else -- 'else'
  | While -- 'while'
  | Until -- 'until'
  | Do -- 'do'

  deriving Show
