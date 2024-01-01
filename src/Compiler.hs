module Compiler where

import Utils.AST ( Aexp, Bexp, Program )
import qualified Utils.AST as AST ( Aexp(..), Bexp(..), Stm(..) )

import Utils.Inst ( Code )
import qualified Utils.Inst as Inst ( Inst(..) )

-- Compiles an arithmetic expression
compA :: Aexp -> Code
compA (AST.I i) = [Inst.Push i]
compA (AST.Var s) = [Inst.Fetch s]
compA (AST.Add lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Add]
compA (AST.Mult lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Mult]
compA (AST.Sub lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Sub]

-- Compiles a boolean expression
compB :: Bexp -> Code
compB (AST.B True) = [Inst.Tru]
compB (AST.B False) = [Inst.Fals]
compB (AST.Lt lhs rhs) = rhs' ++ lhs' ++ [Inst.Le] ++ rhs' ++ lhs' ++ [Inst.Equ, Inst.Neg, Inst.And]
  where
    lhs' = compA lhs
    rhs' = compA rhs
compB (AST.Le lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Le]
compB (AST.Gt lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Le, Inst.Neg]
compB (AST.Ge lhs rhs) = (compB (AST.Lt lhs rhs)) ++ [Inst.Neg]
compB (AST.IEqu lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Equ]
compB (AST.Not exp) = (compB exp) ++ [Inst.Neg]
compB (AST.BEqu lhs rhs) = (compB rhs) ++ (compB lhs) ++ [Inst.Equ]
compB (AST.And lhs rhs) = (compB rhs) ++ (compB lhs) ++ [Inst.And]
compB (AST.Or lhs rhs) = (compB rhs) ++ [Inst.Neg] ++ (compB lhs) ++ [Inst.Neg, Inst.And, Inst.Neg]

-- Compiles a program i.e. a list of statements.
compile :: Program -> Code
compile [] = []
compile ( (AST.Assign var exp):xs ) = (compA exp) ++ [Inst.Store var] ++ (compile xs)
compile ( (AST.If cond c1 []):xs ) = (compB cond) ++ [Inst.Branch (compile c1) [Inst.Noop] ] ++ (compile xs)
compile ( (AST.If cond c1 c2):xs ) = (compB cond) ++ [Inst.Branch (compile c1) (compile c2) ] ++ (compile xs)
compile ( (AST.While c1 c2):xs ) = [Inst.Loop (compB c1) (compile c2) ] ++ (compile xs)
