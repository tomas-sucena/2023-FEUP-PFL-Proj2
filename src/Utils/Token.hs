module Utils.Token where

data Token =
  Var String    -- variables

  -- values
  | I Integer   -- integers
  | B Bool      -- booleans

  -- symbols
  | Semicolon   -- ';'
  | LParen      -- '('
  | RParen      -- ')'

  -- operators
  | Assign      -- ':='
  | Add         -- '+'
  | Mult        -- '*'
  | Sub         -- '-'
  | Le          -- '<='
  | Lt          -- '<'
  | Ge          -- '>='
  | Gt          -- '>'
  | IEqu        -- '=='
  | INequ       -- '!='
  | Not         -- 'not'
  | BEqu        -- '='
  | And         -- 'and'
  | Or          -- 'or'

  -- keywords
  | If          -- 'if'
  | Then        -- 'then'
  | Else        -- 'else'
  | While       -- 'while'
  | Until       -- 'until'
  | Do          -- 'do'

  deriving Show
