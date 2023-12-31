module Parser where

import Utils.Stm ( Aexp, Bexp, Stm, Program )
import qualified Utils.Stm as Stm ( Aexp(..), Bexp(..), Stm(..) )

import Utils.Token ( Token )
import qualified Utils.Token as Token ( Token(..) )

import Lexer (lexer)

-- Parse primary arithmetic expressions.
parsePrimaryA :: [Token] -> (Maybe Aexp, [Token])
parsePrimaryA ( (Token.I i):tokens ) = (Just (Stm.I i), tokens)
parsePrimaryA ( (Token.Var var):tokens ) = (Just (Stm.Var var), tokens)
parsePrimaryA ( Token.LParen:tokens ) =
  case parseAexp tokens of
    (exp, Token.RParen:tokens') -> (exp, tokens')
    _ -> error "Parse error: expected a closing ')'."
parsePrimaryA tokens = (Nothing, tokens)

-- Parse unary arithmetic expressions.
parseUnaryA :: (Maybe Aexp, [Token]) -> (Maybe Aexp, [Token])
parseUnaryA (_, Token.Sub:tokens) =
  case parsePrimaryA tokens of
    (Just exp, tokens') -> parseUnaryA (Just (Stm.Sub (Stm.I 0) exp), tokens')
    _ -> error "Parse error: expected an arithmetic expression after '-'"
parseUnaryA (Nothing, tokens) = parsePrimaryA tokens
parseUnaryA (exp, tokens) = (exp, tokens)

-- Parse factors.
parseFactor :: (Maybe Aexp, [Token]) -> (Maybe Aexp, [Token])

-- multiplication
parseFactor (Just lhs, Token.Mult:tokens) =
  case parseUnaryA (Nothing, tokens) of
    (Just rhs, tokens') -> parseFactor (Just (Stm.Mult lhs rhs), tokens')
    _ -> error "Parse error: expected an arithmetic expression after '*'"

parseFactor (Nothing, tokens) =
  case parseUnaryA (Nothing, tokens) of
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

-- Parse an arithmetic expression.
parseAexp :: [Token] -> (Maybe Aexp, [Token])
parseAexp tokens = parseTerm (Nothing, tokens)

{- BOOLEAN EXPRESSIONS -}
-- Parse integer comparisons.
parseComparisonA :: (Maybe Aexp, [Token]) -> (Maybe Bexp, [Token])

-- less or equal
parseComparisonA (Just lhs, Token.Le:tokens) =
  case parseAexp tokens of
    (Just rhs, tokens') -> (Just (Stm.Le lhs rhs), tokens')
    _ -> error "Parse error: expected an arithmetic expression after '<='"

-- integer equality
parseComparisonA (Just lhs, Token.IEqu:tokens) =
  case parseAexp tokens of
    (Just rhs, tokens') -> (Just (Stm.IEqu lhs rhs), tokens')
    _ -> error "Parse error: expected an arithmetic expression after '=='"

parseComparisonA (Nothing, tokens) =
  case parseAexp tokens of
    (Nothing, tokens') -> (Nothing, tokens')
    (exp, tokens') -> parseComparisonA (exp, tokens')
parseComparisonA (_, tokens) = (Nothing, tokens)

-- Parse primary boolean expressions.
-- NOTE: For simplicity, integer comparisons are treated as primary boolean expressions, because both have the same precedence.
parsePrimaryB :: [Token] -> (Maybe Bexp, [Token])
parsePrimaryB ( (Token.B True):tokens ) = (Just (Stm.B True), tokens)
parsePrimaryB ( (Token.B False):tokens ) = (Just (Stm.B False), tokens)
parsePrimaryB ( Token.LParen:tokens ) =
  case parseBexp tokens of
    (exp, Token.RParen:tokens') -> (exp, tokens')
    _ -> error "Parse error: expected a closing ')'."
parsePrimaryB tokens = parseComparisonA (Nothing, tokens)

-- Parse unary boolean expressions.
parseUnaryB :: (Maybe Bexp, [Token]) -> (Maybe Bexp, [Token])
parseUnaryB (_, Token.Not:tokens) =
  case parsePrimaryB tokens of
    (Just exp, tokens') -> parseUnaryB (Just (Stm.Not exp), tokens')
    _ -> error "Parse error: expected a boolean expression after 'not'"
parseUnaryB (Nothing, tokens) = parsePrimaryB tokens
parseUnaryB (exp, tokens) = (exp, tokens)

-- Parse boolean comparisons.
parseComparisonB :: (Maybe Bexp, [Token]) -> (Maybe Bexp, [Token])

-- boolean equality
parseComparisonB (Just lhs, Token.BEqu:tokens) =
  case parseUnaryB (Nothing, tokens) of
    (Just rhs, tokens') -> (Just (Stm.BEqu lhs rhs), tokens')
    _ -> error "Parse error: expected a boolean expression after '='"

parseComparisonB (Nothing, tokens) =
  case parseUnaryB (Nothing, tokens) of
    (Nothing, tokens') -> (Nothing, tokens')
    (exp, tokens') -> parseComparisonB (exp, tokens')
parseComparisonB (exp, tokens) = (exp, tokens)

-- Parse boolean logical expressions.
parseLogical :: (Maybe Bexp, [Token]) -> (Maybe Bexp, [Token])

-- logical AND
parseLogical (Just lhs, Token.And:tokens) =
  case parseComparisonB (Nothing, tokens) of
    (Just rhs, tokens') -> (Just (Stm.And lhs rhs), tokens')
    _ -> error "Parse error: expected a boolean expression after '='"

parseLogical (Nothing, tokens) =
  case parseComparisonB (Nothing, tokens) of
    (Nothing, tokens') -> (Nothing, tokens')
    (exp, tokens') -> parseLogical (exp, tokens')
parseLogical (exp, tokens) = (exp, tokens)

-- Parse a boolean expression.
parseBexp :: [Token] -> (Maybe Bexp, [Token])
parseBexp tokens = parseLogical (Nothing, tokens)

{- STATEMENTS -}
-- Parse statements.
parseStm :: (Maybe Stm, [Token]) -> (Maybe Stm, [Token])

-- assignment
parseStm (Nothing, (Token.Var var):(Token.Assign):tokens) =
  case parseAexp tokens of
    (Just exp, Token.Semicolon:tokens') -> (Just (Stm.Assign var exp), tokens')
    (_, Token.Semicolon:tokens') -> error "Parse error: expected an arithmetic expression"
    _ -> error "Parse error: expected a semicolon after an assignment"

-- if statements
parseStm (Nothing, Token.If:tokens) =
  case parseBexp tokens of
    (Just cond, Token.Then:tokens') -> parseStm (Just (Stm.If cond [] []), tokens')
    (Just exp, _) -> error "Parse error: expected a 'then' after an 'if'"
    _ -> error "Parse error: expected a boolean expression after an 'if'"

parseStm (Just (Stm.If cond [] []), tokens) =
  case parseStm (Nothing, tokens) of
    (Just c1, tokens') -> parseStm (Just (Stm.If cond [c1] []), tokens')
    _ -> error "Parse error: expected a statement after an 'if'"

parseStm (Just (Stm.If cond c1 []), Token.Else:tokens) =
  case parseStm (Nothing, tokens) of
    (Just c2, tokens') -> parseStm (Just (Stm.If cond c1 [c2]), tokens')
    _ -> error "Parse error: expected a statement after an 'if'"

parseStm (stm, tokens) = (stm, tokens)

parseB :: String -> Bexp
parseB s = exp
  where (Just exp, _) = parseBexp (lexer s)

-- Parses code and outputs a list of statements.
parse :: String -> Program
parse s =
  case parseStm (Nothing, (lexer s)) of
    (Just stm, _) -> [stm]
