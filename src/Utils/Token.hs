module Utils.Token where

data Token =
  -- values
  IntTok Integer
  | VarTok String

  -- operators
  | AssignTok -- ':='
  | NotTok -- '~'
  | PlusTok -- '+'
  | MinusTok -- '-'
  | TimesTok -- '*'
  | EqTok -- '='
  | EqEqTok -- '=='

  -- symbols
  | SemicolonTok -- ';'
  | LParenTok -- '('
  | RParenTok -- ')'

  -- keywords
  | IfTok -- 'if'
  | ElseTok -- 'else'
  | WhileTok -- 'while'
  | UntilTok -- 'until'
  | DoTok -- 'do'
  | ThenTok -- 'then'
