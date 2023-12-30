module Parser where

import Utils.Stm ( Aexp, Bexp, Stm, Program )
import qualified Utils.Stm as Stm ( Aexp(..), Bexp(..), Stm(..) )

import Utils.Token ( Token )
import qualified Utils.Token as Token ( Token(..) )

import Lexer (lexer)

-- Parse arithmetic expressions.
parseAexp :: [Token] -> (Aexp, [Token])

-- addition
parseAexp ( (Token.I i):(Token.Add):tokens ) = (Stm.Add rhs (Stm.I i), tokens')
  where (rhs, tokens') = parseAexp tokens
parseAexp ( (Token.Var var):(Token.Add):tokens ) = (Stm.Add rhs (Stm.Var var), tokens')
  where (rhs, tokens') = parseAexp tokens

-- literals
parseAexp ((Token.I i):tokens) = (Stm.I i, tokens)
parseAexp ((Token.Var var):tokens) = (Stm.Var var, tokens)

-- Parse a list of tokens.
parseTokens :: [Token] -> Program
parseTokens [] = []

-- assignment
parseTokens ((Token.Var var):(Token.Assign):tokens) =
  case parseAexp tokens of
    (exp, Token.Semicolon:tokens') -> (Stm.Assign var exp):(parseTokens tokens')
    _ -> error "Parse error: expected a semicolon after an assignment"

-- Parses code and outputs a list of statements.
parse :: String -> Program
parse s = parseTokens (lexer s)
