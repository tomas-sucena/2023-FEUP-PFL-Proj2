module Compiler where

import Utils.Stm ( Aexp, Bexp, Program )
import qualified Utils.Stm as Stm ( Aexp(..), Bexp(..), Stm(..) )

import Utils.Inst ( Code )
import qualified Utils.Inst as Inst ( Inst(..) )

-- Compiles an arithmetic expression
compA :: Aexp -> Code
compA (Stm.I i) = [Inst.Push i]
compA (Stm.Var s) = [Inst.Fetch s]
compA (Stm.Add lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Add]
compA (Stm.Mult lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Mult]
compA (Stm.Sub lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Sub]

-- Compiles a boolean expression
compB :: Bexp -> Code
compB (Stm.B True) = [Inst.Tru]
compB (Stm.B False) = [Inst.Fals]
compB (Stm.Lt lhs rhs) = rhs' ++ lhs' ++ [Inst.Le] ++ rhs' ++ lhs' ++ [Inst.Equ, Inst.Neg, Inst.And]
  where
    lhs' = compA lhs
    rhs' = compA rhs
compB (Stm.Le lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Le]
compB (Stm.Gt lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Le, Inst.Neg]
compB (Stm.Ge lhs rhs) = (compB (Stm.Lt lhs rhs)) ++ [Inst.Neg]
compB (Stm.IEqu lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Equ]
compB (Stm.Not exp) = (compB exp) ++ [Inst.Neg]
compB (Stm.BEqu lhs rhs) = (compB rhs) ++ (compB lhs) ++ [Inst.Equ]
compB (Stm.And lhs rhs) = (compB rhs) ++ (compB lhs) ++ [Inst.And]
compB (Stm.Or lhs rhs) = (compB rhs) ++ [Inst.Neg] ++ (compB lhs) ++ [Inst.Neg, Inst.And, Inst.Neg]

-- Compiles a program i.e. a list of statements.
compile :: Program -> Code
compile [] = []
compile ( (Stm.Assign var exp):xs ) = (compA exp) ++ [Inst.Store var] ++ (compile xs)
compile ( (Stm.If cond c1 []):xs ) = (compB cond) ++ [Inst.Branch (compile c1) [Inst.Noop] ] ++ (compile xs)
compile ( (Stm.If cond c1 c2):xs ) = (compB cond) ++ [Inst.Branch (compile c1) (compile c2) ] ++ (compile xs)
compile ( (Stm.While c1 c2):xs ) = [Inst.Loop (compB c1) (compile c2) ] ++ (compile xs)
