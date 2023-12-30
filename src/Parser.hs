module Parser where

import qualified Utils.Stm as Stm ( Aexp(..), Bexp(..), Stm(..), Program )

import Utils.Token ( Token )
import qualified Utils.Token as Token ( Token(..) )

import Lexer (lexer)

parseAexp :: [Tokens] -> (Aexp, [Tokens])
parseAexp ((Token.I i):tokens) = (Stm.I i, tokens)

parseAssignment :: (String, [Tokens]) -> (Stm, [Tokens])
-- parseAssignment (var, (Assign):tokens)
parseAssignment (var, _) = error "Parse error: expected an assignment after " ++ var

parseTokens :: [Token] -> Program
parseTokens [] = []

-- parse an assignment
--parseTokens (Var s):(Assign):tokens = []
parseTokens ((Token.Var var):tokens) = stm:(parseTokens tokens')
  where (stm, tokens') = parseAssignment (var, tokens)

-- Parses code and outputs a list of statements.
parse :: String -> Program
parse s = parseTokens (lexer s)
