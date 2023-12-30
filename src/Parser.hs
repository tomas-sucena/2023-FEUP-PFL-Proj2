module Parser where

import Utils.Stm ( Aexp, Bexp, Stm, Program )
import qualified Utils.Stm as Stm ( Aexp(..), Bexp(..), Stm(..) )

import Utils.Token ( Token )
import qualified Utils.Token as Token ( Token(..) )

import Lexer (lexer)

-- Parse primary expressions.
parsePrimary :: [Token] -> (Maybe Aexp, [Token])
parsePrimary ( (Token.I i):tokens ) = (Just (Stm.I i), tokens)
parsePrimary ( (Token.Var var):tokens ) = (Just (Stm.Var var), tokens)
parsePrimary tokens = (Nothing, tokens)

-- Parse factors.
parseFactor :: (Maybe Aexp, [Token]) -> (Maybe Aexp, [Token])

-- multiplication
parseFactor (Just lhs, Token.Mult:tokens) =
  case parsePrimary tokens of
    (Just rhs, tokens') -> parseFactor (Just (Stm.Mult lhs rhs), tokens')
    _ -> error "Parse error: expected an arithmetic expression after '*'"
parseFactor (exp, tokens) =
  case parsePrimary tokens of
    (Nothing, tokens') -> (exp, tokens')
    (exp', tokens') -> parseFactor (exp', tokens')

-- Parse a list of tokens.
parseTokens :: [Token] -> Program
parseTokens [] = []

-- assignment
parseTokens ((Token.Var var):(Token.Assign):tokens) =
  case parseFactor (Nothing, tokens) of
    (Just exp, Token.Semicolon:tokens') -> (Stm.Assign var exp):(parseTokens tokens')
    (_, Token.Semicolon:tokens') -> error "Parse error: expected an arithmetic expression"
    _ -> error "Parse error: expected a semicolon after an assignment"

-- Parses code and outputs a list of statements.
parse :: String -> Program
parse s = parseTokens (lexer s)
