module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit, isLower)
import Utils.Token as Token ( Token(..) )

-- Lexes the first word that appears in the string.
lexNumber :: String -> (Token, String)
lexNumber s = (Token.Int (read number :: Integer), s')
  where
    (number, s') = break (not . isDigit) s

-- Lexes the first word that appears in the string.
lexWord :: String -> (Token, String)
lexWord s = (token, s')
  where
    (word, s') = break (not . isAlphaNum) s
    token = case word of
      "if" -> Token.If
      "then" -> Token.Then
      "else" -> Token.Else
      "while" -> Token.While
      "until" -> Token.Until
      "do" -> Token.Do
      _ -> Token.Var word

-- Converts the source code into a list of tokens.
lexer :: String -> [Token]
lexer [] = []

-- whitespace
lexer (' ':s) = lexer s
lexer ('\n':s) = lexer s
lexer ('\t':s) = lexer s
lexer ('\r':s) = lexer s

-- symbols
lexer (';':s) = Token.Semicolon:(lexer s)
lexer ('(':s) = Token.LParen:(lexer s)
lexer (')':s) = Token.RParen:(lexer s)

-- operators
lexer (':':'=':s) = Token.Assign:(lexer s)
lexer ('=':'=':s) = Token.EqEq:(lexer s)
lexer ('~':s) = Token.Not:(lexer s)
lexer ('+':s) = Token.Plus:(lexer s)
lexer ('-':s) = Token.Minus:(lexer s)
lexer ('*':s) = Token.Times:(lexer s)
lexer ('=':s) = Token.Eq:(lexer s)

lexer s@(c:_)
  | isDigit c = token : lexer s'
  where (token, s') = lexNumber s

lexer s@(c:_)
  | isAlpha c && isLower c = token : lexer s'
  where (token, s') = lexWord s

lexer (c:_) = error ("Unexpected character: '" ++ show c ++ "'")