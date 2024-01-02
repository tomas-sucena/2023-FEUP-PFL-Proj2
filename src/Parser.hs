module Parser where

import Utils.AST ( Aexp, Bexp, Stm, Program )
import qualified Utils.AST as AST ( Aexp(..), Bexp(..), Stm(..) )

import Utils.Token ( Token )
import qualified Utils.Token as Token ( Token(..) )

import Lexer (lexer)

-- Parse primary arithmetic expressions.
parsePrimaryA :: [Token] -> (Maybe Aexp, [Token])
parsePrimaryA ( (Token.I i):tokens ) = (Just (AST.I i), tokens)
parsePrimaryA ( (Token.Var var):tokens ) = (Just (AST.Var var), tokens)
parsePrimaryA ( Token.LParen:tokens ) =
  case parseAexp tokens of
    (exp, Token.RParen:tokens') -> (exp, tokens')
    _ -> error "Parse error - expected a closing ')'."
parsePrimaryA tokens = (Nothing, tokens)

-- Parse unary arithmetic expressions.
parseUnaryA :: (Maybe Aexp, [Token]) -> (Maybe Aexp, [Token])
parseUnaryA (_, Token.Sub:tokens) =
  case parsePrimaryA tokens of
    (Just exp, tokens') -> parseUnaryA (Just (AST.Sub (AST.I 0) exp), tokens')
    _ -> error "Parse error - expected an arithmetic expression after '-'"
parseUnaryA (Nothing, tokens) = parsePrimaryA tokens
parseUnaryA (exp, tokens) = (exp, tokens)

-- Parse factors.
parseFactor :: (Maybe Aexp, [Token]) -> (Maybe Aexp, [Token])

-- multiplication
parseFactor (Just lhs, Token.Mult:tokens) =
  case parseUnaryA (Nothing, tokens) of
    (Just rhs, tokens') -> parseFactor (Just (AST.Mult lhs rhs), tokens')
    _ -> error "Parse error - expected an arithmetic expression after '*'"

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
      (Just rhs, tokens') -> parseTerm (Just (AST.Add lhs rhs), tokens')
      _ -> error "Parse error - expected an arithmetic expression after '+'"

-- subtraction
parseTerm (Just lhs, Token.Sub:tokens) =
    case parseFactor (Nothing, tokens) of
      (Just rhs, tokens') -> parseTerm (Just (AST.Sub lhs rhs), tokens')
      _ -> error "Parse error - expected an arithmetic expression after '-'"

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
    (Just rhs, tokens') -> (Just (AST.Le lhs rhs), tokens')
    _ -> error "Parse error - expected an arithmetic expression after '<='"

-- less than
parseComparisonA (Just lhs, Token.Lt:tokens) =
  case parseAexp tokens of
    (Just rhs, tokens') -> (Just (AST.Lt lhs rhs), tokens')
    _ -> error "Parse error - expected an arithmetic expression after '<'"

-- greater than
parseComparisonA (Just lhs, Token.Gt:tokens) =
  case parseAexp tokens of
    (Just rhs, tokens') -> (Just (AST.Gt lhs rhs), tokens')
    _ -> error "Parse error - expected an arithmetic expression after '>'"

-- greater equal
parseComparisonA (Just lhs, Token.Ge:tokens) =
  case parseAexp tokens of
    (Just rhs, tokens') -> (Just (AST.Ge lhs rhs), tokens')
    _ -> error "Parse error - expected an arithmetic expression after '>='"

-- integer equality
parseComparisonA (Just lhs, Token.IEqu:tokens) =
  case parseAexp tokens of
    (Just rhs, tokens') -> (Just (AST.IEqu lhs rhs), tokens')
    _ -> error "Parse error - expected an arithmetic expression after '=='"

-- integer inequality
parseComparisonA (Just lhs, Token.INequ:tokens) =
  case parseAexp tokens of
    (Just rhs, tokens') -> (Just (AST.INequ lhs rhs), tokens')
    _ -> error "Parse error - expected an arithmetic expression after '!='"

parseComparisonA (Nothing, tokens) =
  case parseAexp tokens of
    (Nothing, tokens') -> (Nothing, tokens')
    (exp, tokens') -> parseComparisonA (exp, tokens')
parseComparisonA (_, tokens) = (Nothing, tokens)

-- Parse primary boolean expressions.
-- NOTE: For simplicity, integer comparisons are treated as primary boolean expressions, because both have the same precedence.
parsePrimaryB :: [Token] -> (Maybe Bexp, [Token])
parsePrimaryB ( (Token.B True):tokens ) = (Just (AST.B True), tokens)
parsePrimaryB ( (Token.B False):tokens ) = (Just (AST.B False), tokens)
parsePrimaryB ( Token.LParen:tokens ) =
  case parseBexp tokens of
    (exp, Token.RParen:tokens') -> (exp, tokens')
    _ -> error "Parse error - expected a closing ')'."
parsePrimaryB tokens = parseComparisonA (Nothing, tokens)

-- Parse unary boolean expressions.
parseUnaryB :: (Maybe Bexp, [Token]) -> (Maybe Bexp, [Token])
parseUnaryB (_, Token.Not:tokens) =
  case parsePrimaryB tokens of
    (Just exp, tokens') -> parseUnaryB (Just (AST.Not exp), tokens')
    _ -> error "Parse error - expected a boolean expression after 'not'"
parseUnaryB (Nothing, tokens) = parsePrimaryB tokens
parseUnaryB (exp, tokens) = (exp, tokens)

-- Parse boolean comparisons.
parseComparisonB :: (Maybe Bexp, [Token]) -> (Maybe Bexp, [Token])

-- boolean equality
parseComparisonB (Just lhs, Token.BEqu:tokens) =
  case parseUnaryB (Nothing, tokens) of
    (Just rhs, tokens') -> (Just (AST.BEqu lhs rhs), tokens')
    _ -> error "Parse error - expected a boolean expression after '='"

parseComparisonB (Nothing, tokens) =
  case parseUnaryB (Nothing, tokens) of
    (Nothing, tokens') -> (Nothing, tokens')
    (exp, tokens') -> parseComparisonB (exp, tokens')
parseComparisonB (exp, tokens) = (exp, tokens)

-- Parse boolean conjunction i.e. logical AND.
parseConjuntion :: (Maybe Bexp, [Token]) -> (Maybe Bexp, [Token])

-- logical AND
parseConjuntion (Just lhs, Token.And:tokens) =
  case parseComparisonB (Nothing, tokens) of
    (Just rhs, tokens') -> (Just (AST.And lhs rhs), tokens')
    _ -> error "Parse error - expected a boolean expression after 'and'"

parseConjuntion (Nothing, tokens) =
  case parseComparisonB (Nothing, tokens) of
    (Nothing, tokens') -> (Nothing, tokens')
    (exp, tokens') -> parseConjuntion (exp, tokens')
parseConjuntion (exp, tokens) = (exp, tokens)

-- Parse boolean disjunction i.e. logical OR.
parseDisjunction :: (Maybe Bexp, [Token]) -> (Maybe Bexp, [Token])

-- logical OR
parseDisjunction (Just lhs, Token.Or:tokens) =
  case parseConjuntion (Nothing, tokens) of
    (Just rhs, tokens') -> (Just (AST.Or lhs rhs), tokens')
    _ -> error "Parse error - expected a boolean expression after 'or'"

parseDisjunction (Nothing, tokens) =
  case parseConjuntion (Nothing, tokens) of
    (Nothing, tokens') -> (Nothing, tokens')
    (exp, tokens') -> parseDisjunction (exp, tokens')
parseDisjunction (exp, tokens) = (exp, tokens)

-- Parse a boolean expression.
parseBexp :: [Token] -> (Maybe Bexp, [Token])
parseBexp tokens = parseDisjunction (Nothing, tokens)

{- STATEMENTS -}
-- Parse a statement or a group of statements delimited by parenthesis.
parseStm :: ([Stm], [Token]) -> ([Stm], [Token])

-- assignment
parseStm ([], (Token.Var var):(Token.Assign):tokens) =
  case parseAexp tokens of
    (Just exp, Token.Semicolon:tokens') -> ( [AST.Assign var exp], tokens')
    (_, Token.Semicolon:tokens') -> error "Parse error - expected an arithmetic expression"
    _ -> error "Parse error - expected a semicolon after an assignment"

-- 'if' statements
-- Parse the condition of an 'if' statement.
parseStm ([], Token.If:tokens) =
  case parseBexp tokens of
    (Just cond, Token.Then:tokens') -> parseStm ( [ AST.If cond [] [] ], tokens')
    (Just exp, _) -> error "Parse error - expected a 'then' after an 'if'"
    _ -> error "Parse error - expected a boolean expression after an 'if'"

-- Parse the code of an 'if' statement.
parseStm ( [ AST.If cond [] [] ], tokens) =
  case parseStm ([], tokens) of
    ([], _) -> error "Parse error - expected a statement after an 'if'"
    (c1, tokens') -> parseStm ( [ AST.If cond c1 [] ], tokens')

-- Parse the code of an 'else' statement.
parseStm ( [ AST.If cond c1 [] ], Token.Else:tokens) =
  case parseStm ([], tokens) of
    ([], _) -> error "Parse error - expected a statement after an 'else'"
    (c2, tokens') -> ( [AST.If cond c1 c2], tokens')

parseStm ( [ AST.If cond c1 [] ], tokens) = ( [ AST.If cond c1 [] ], tokens)

-- loop statements
-- Parse the condition of a 'while' statement.
parseStm ([], Token.While:tokens) =
  case parseBexp tokens of
    (Just cond, Token.Do:tokens') -> parseStm ( [ AST.While cond [] ], tokens')
    (Just exp, _) -> error "Parse error - expected a 'do' after a 'while'"
    _ -> error "Parse error - expected a boolean expression after a 'while'"

-- Parse the condition of an 'until' statement.
parseStm ([], Token.Until:tokens) =
  case parseBexp tokens of
    (Just cond, Token.Do:tokens') -> parseStm ( [ AST.While (AST.Not cond) [] ], tokens')
    (Just exp, _) -> error "Parse error - expected a 'do' after a 'while'"
    _ -> error "Parse error - expected a boolean expression after a 'while'"

-- Parse the code of a 'while' statement.
parseStm ( [ AST.While cond [] ], tokens) =
  case parseStm ([], tokens) of
    ([], _) -> error "Parse error - expected a statement after a 'while'"
    (c1, tokens') -> ( [AST.While cond c1], tokens')

-- Parse multiple statements.
parseStm ([], Token.LParen:tokens) =
  case parseStm([], tokens) of
    ([], _) -> error "Parse error - expected a statement after '('"
    (stm, Token.RParen:Token.Semicolon:tokens') -> (stm, tokens')
    (stm, Token.RParen:tokens') -> (stm, tokens')
    (stm, tokens') -> parseStm (stm, tokens')

parseStm([], _) = error "Parse error - unexpected statement"

parseStm (stms, tokens) =
  case parseStm([], tokens) of
    (stm, Token.RParen:Token.Semicolon:tokens') -> (stms ++ stm, tokens')
    (stm, Token.RParen:tokens') -> (stms ++ stm, tokens')
    (stm, tokens') -> parseStm (stms ++ stm, tokens')

-- Parses the tokens generated by the lexer.
parseTokens :: [Token] -> Program
parseTokens [] = []
parseTokens tokens = stm ++ (parseTokens tokens')
  where (stm, tokens') = parseStm ([], tokens)

-- Parses code and outputs a list of statements.
parse :: String -> Program
parse s = parseTokens (lexer s)
