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

parseFactor (Nothing, tokens) =
  case parsePrimary tokens of
    (Nothing, tokens') -> (Nothing, tokens')
    (exp, tokens') -> parseFactor (exp, tokens')
parseFactor (exp, tokens) = (exp, tokens)

-- Parse terms.
parseTerm :: (Maybe Aexp, [Token]) -> (Maybe Aexp, [Token])

-- addition
parseTerm (Just lhs, Token.Add:tokens) =
    case parseFactor (Nothing, tokens) of
      (Just rhs, tokens') -> parseTerm (Just (Stm.Add lhs rhs), tokens')
      _ -> error "Parse error: expected an arithmetic expression after '+'"

-- subtraction
parseTerm (Just lhs, Token.Sub:tokens) =
    case parseFactor (Nothing, tokens) of
      (Just rhs, tokens') -> parseTerm (Just (Stm.Sub lhs rhs), tokens')
      _ -> error "Parse error: expected an arithmetic expression after '-'"

parseTerm (Nothing, tokens) =
  case parseFactor (Nothing, tokens) of
    (Nothing, tokens') -> (Nothing, tokens')
    (exp, tokens') -> parseTerm (exp, tokens')

parseTerm (exp, tokens) = (exp, tokens)

-- Parse a list of tokens.
parseTokens :: [Token] -> Program
parseTokens [] = []

-- assignment
parseTokens ((Token.Var var):(Token.Assign):tokens) =
  case parseTerm (Nothing, tokens) of
    (Just exp, Token.Semicolon:tokens') -> (Stm.Assign var exp):(parseTokens tokens')
    (_, Token.Semicolon:tokens') -> error "Parse error: expected an arithmetic expression"
    _ -> error "Parse error: expected a semicolon after an assignment"

-- Parses code and outputs a list of statements.
parse :: String -> Program
parse s = parseTokens (lexer s)
