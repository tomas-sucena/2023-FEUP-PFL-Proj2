module Utils.Token where

data Token =
  -- values
  IntTok Integer
  | VarTok String

  -- symbols
  | SemicolonTok -- ';'
  | LParenTok -- '('
  | RParenTok -- ')'

  -- operators
  | AssignTok -- ':='
  | NotTok -- '~'
  | PlusTok -- '+'
  | MinusTok -- '-'
  | TimesTok -- '*'
  | EqTok -- '='
  | EqEqTok -- '=='

  -- keywords
  | IfTok -- 'if'
  | ThenTok -- 'then'
  | ElseTok -- 'else'
  | WhileTok -- 'while'
  | UntilTok -- 'until'
  | DoTok -- 'do'

  deriving Show
