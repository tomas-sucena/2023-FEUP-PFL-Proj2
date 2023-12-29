module Compiler where

import Utils.Stm ( Aexp, Bexp, Program )
import qualified Utils.Stm as Stm ( Aexp(..), Bexp(..), Stm(..) )

import Utils.Inst ( Code )
import qualified Utils.Inst as Inst ( Inst(..) )

-- Compiles an arithmetic expression
compA :: Aexp -> Code
compA (Stm.Int i) = [Inst.Push i]
compA (Stm.Var s) = [Inst.Fetch s]
compA (Stm.Add lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Add]
compA (Stm.Mult lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Mult]
compA (Stm.Sub lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Sub]

-- Compiles a boolean expression
compB :: Bexp -> Code
compB (Stm.Bool True) = [Inst.Tru]
compB (Stm.Bool False) = [Inst.Fals]
compB (Stm.Not exp) = (compB exp) ++ [Inst.Neg]
compB (Stm.And lhs rhs) = (compB rhs) ++ (compB lhs) ++ [Inst.And]
compB (Stm.BEq lhs rhs) = (compB rhs) ++ (compB lhs) ++ [Inst.Equ]
compB (Stm.IEq lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Inst.Equ]

-- Compiles a list of statements.
comp :: Program -> Code
comp [] = []
comp ( (Stm.Assign var exp):xs ) = (compA exp) ++ [Inst.Store var] ++ (comp xs)
comp ( (Stm.If cond code):xs ) = [Inst.Branch (compB cond) (comp [code]) ] ++ (comp xs)
comp ( (Stm.While cond code):xs ) = [Inst.Loop (compB cond) (comp [code]) ] ++ (comp xs)
