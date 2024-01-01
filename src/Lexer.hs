module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit, isLower)
import Utils.Token as Token ( Token(..) )

-- Lexes a decimal number.
lexDecNumber :: String -> (Token, String)
lexDecNumber s = (Token.I (read number :: Integer), s')
  where
    (number, s') = break (not . isDigit) s

-- Lexes a binary number
lexBinNumber :: (Token, String) -> (Token, String)
lexBinNumber (token, []) = (token, [])
lexBinNumber (Token.I i, '0':s) = lexBinNumber (Token.I (i * 2), s)
lexBinNumber (Token.I i, '1':s) = lexBinNumber (Token.I (i * 2 + 1), s)
lexBinNumber (Token.I i, c:s)
  | isAlphaNum c = error ("Lex error - unexpected digit " ++ show c ++ " in binary number.")
  | otherwise = (Token.I i, c:s)

-- Lexes an octal number
lexOctalNumber :: (Token, String) -> (Token, String)
lexOctalNumber (token, []) = (token, [])
lexOctalNumber (Token.I i, c:s)
  | isDigit c && c < '9' = lexOctalNumber (Token.I (i * 8 + read [c]), s)
  | isAlphaNum c = error ("Lex error - unexpected digit " ++ show c ++ " in octal number.")
  | otherwise = (Token.I i, c:s)

-- Lexes a hexadecimal number
lexHexNumber :: (Token, String) -> (Token, String)
lexHexNumber (token, []) = (token, [])
lexHexNumber (Token.I i, 'A':s) = lexHexNumber (Token.I (i * 16 + 10), s)
lexHexNumber (Token.I i, 'a':s) = lexHexNumber (Token.I (i * 16 + 10), s)
lexHexNumber (Token.I i, 'B':s) = lexHexNumber (Token.I (i * 16 + 11), s)
lexHexNumber (Token.I i, 'b':s) = lexHexNumber (Token.I (i * 16 + 11), s)
lexHexNumber (Token.I i, 'C':s) = lexHexNumber (Token.I (i * 16 + 12), s)
lexHexNumber (Token.I i, 'c':s) = lexHexNumber (Token.I (i * 16 + 12), s)
lexHexNumber (Token.I i, 'D':s) = lexHexNumber (Token.I (i * 16 + 13), s)
lexHexNumber (Token.I i, 'd':s) = lexHexNumber (Token.I (i * 16 + 13), s)
lexHexNumber (Token.I i, 'E':s) = lexHexNumber (Token.I (i * 16 + 14), s)
lexHexNumber (Token.I i, 'e':s) = lexHexNumber (Token.I (i * 16 + 14), s)
lexHexNumber (Token.I i, 'F':s) = lexHexNumber (Token.I (i * 16 + 15), s)
lexHexNumber (Token.I i, 'f':s) = lexHexNumber (Token.I (i * 16 + 15), s)
lexHexNumber (Token.I i, c:s)
  | isDigit c = lexHexNumber (Token.I (i * 16 + read [c]), s)
  | isAlpha c = error ("Lex error - unexpected digit " ++ show c ++ " in hexadecimal number.")
  | otherwise = (Token.I i, c:s)

-- Lexes the first word that appears in the string.
lexWord :: String -> (Token, String)
lexWord s = (token, s')
  where
    (word, s') = break (not . isAlphaNum) s
    token = case word of
      "not" -> Token.Not
      "and" -> Token.And
      "or" -> Token.Or
      "if" -> Token.If
      "then" -> Token.Then
      "else" -> Token.Else
      "while" -> Token.While
      "until" -> Token.Until
      "do" -> Token.Do
      "True" -> Token.B True
      "False" -> Token.B False
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
lexer ('<':'=':s) = Token.Le:(lexer s)
lexer ('=':'=':s) = Token.IEqu:(lexer s)
lexer ('+':s) = Token.Add:(lexer s)
lexer ('*':s) = Token.Mult:(lexer s)
lexer ('-':s) = Token.Sub:(lexer s)
lexer ('=':s) = Token.BEqu:(lexer s)

-- numbers
lexer ('0':'b':s) = token : lexer s'
  where (token, s') = lexBinNumber (Token.I 0, s)
lexer ('0':'B':s) = token : lexer s'
  where (token, s') = lexBinNumber (Token.I 0, s)

lexer ('0':'o':s) = token : lexer s'
  where (token, s') = lexOctalNumber (Token.I 0, s)
lexer ('0':'O':s) = token : lexer s'
  where (token, s') = lexOctalNumber (Token.I 0, s)

lexer ('0':'x':s) = token : lexer s'
  where (token, s') = lexHexNumber (Token.I 0, s)
lexer ('0':'X':s) = token : lexer s'
  where (token, s') = lexHexNumber (Token.I 0, s)

lexer s@(c:_)
  | isDigit c = token : lexer s'
  where (token, s') = lexDecNumber s

-- words
lexer s@(c:_)
  | isAlpha c = token : lexer s'
  where (token, s') = lexWord s

lexer (c:_) = error ("Lex error - unexpected character " ++ show c)
