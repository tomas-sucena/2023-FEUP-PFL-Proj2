module Lexer where

import Data.Char (digitToInt, isAlpha, isAlphaNum, isDigit, isLower)
import Utils.Token ( Token(..) )

-- Lexes the first word that appears in the string.
lexNumber :: String -> (Token, String)
lexNumber s = (IntTok (read number :: Integer), s')
  where
    (number, s') = break (not . isDigit) s

-- Lexes the first word that appears in the string.
lexWord :: String -> (Token, String)
lexWord s = (token, s')
  where
    (word, s') = break (not . isAlphaNum) s
    token = case word of
      "if" -> IfTok
      "then" -> ThenTok
      "else" -> ElseTok
      "while" -> WhileTok
      "until" -> UntilTok
      "do" -> DoTok
      _ -> VarTok word

-- Converts the source code into a list of tokens.
lexer :: String -> [Token]
lexer [] = []

-- whitespace
lexer (' ':s) = lexer s
lexer ('\n':s) = lexer s
lexer ('\t':s) = lexer s
lexer ('\r':s) = lexer s

-- symbols
lexer (';':s) = SemicolonTok:(lexer s)
lexer ('(':s) = LParenTok:(lexer s)
lexer (')':s) = RParenTok:(lexer s)

-- operators
lexer (':':'=':s) = AssignTok:(lexer s)
lexer ('=':'=':s) = EqEqTok:(lexer s)
lexer ('~':s) = NotTok:(lexer s)
lexer ('+':s) = PlusTok:(lexer s)
lexer ('-':s) = MinusTok:(lexer s)
lexer ('*':s) = TimesTok:(lexer s)
lexer ('=':s) = EqTok:(lexer s)

lexer s@(c:_)
  | isDigit c = token : lexer s'
  where (token, s') = lexNumber s

lexer s@(c:_)
  | isAlpha c && isLower c = token : lexer s'
  where (token, s') = lexWord s

lexer (c:_) = error ("Unexpected character: '" ++ show c ++ "'")
