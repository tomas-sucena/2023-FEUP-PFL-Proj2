module Parser where

import Utils.Stm ( Aexp, Bexp, Stm, Program )
import qualified Utils.Stm as Stm ( Aexp(..), Bexp(..), Stm(..) )

import Utils.Token ( Token )
import qualified Utils.Token as Token ( Token(..) )

import Lexer (lexer)

-- Parse arithmetic expressions.
parseAexp :: (Maybe Aexp, [Token]) -> (Maybe Aexp, [Token])

-- multiplication
parseAexp ( Just lhs, Token.Mult:tokens ) =
  case parseAexp (Nothing, tokens) of
    (Just rhs, tokens') -> ( Just (Stm.Mult lhs rhs), tokens')
    _ -> error "hey"
parseAexp ( _, Token.Mult:tokens ) = error "Parse error: expected an arithmetic expression before '*'"

-- literals
parseAexp (_, (Token.I i):tokens) = parseAexp (Just (Stm.I i), tokens)
parseAexp (_, (Token.Var var):tokens) = parseAexp (Just (Stm.Var var), tokens)

parseAexp (exp, tokens) = (exp, tokens)

-- Parse a list of tokens.
parseTokens :: [Token] -> Program
parseTokens [] = []

-- assignment
parseTokens ((Token.Var var):(Token.Assign):tokens) =
  case parseAexp (Nothing, tokens) of
    (Just exp, Token.Semicolon:tokens') -> (Stm.Assign var exp):(parseTokens tokens')
    (_, Token.Semicolon:tokens') -> error "Parse error: expected an arithmetic expression"
    _ -> error "Parse error: expected a semicolon after an assignment"

-- Parses code and outputs a list of statements.
parse :: String -> Program
parse s = parseTokens (lexer s)
