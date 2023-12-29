module Compiler where

import Utils.Exp ( Aexp(..), Bexp(..), Stm(..) )
import Utils.Inst ( Inst(..), Code )

-- Compiles an arithmetic expression
compA :: Aexp -> Code
compA (Int i) = [Push i]
compA (Var s) = [Fetch s]
compA (Add lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Add]
compA (Mult lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Mult]
compA (Sub lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Sub]

-- Compiles a boolean expression
compB :: Bexp -> Code
compB (Bool True) = [Tru]
compB (Bool False) = [Fals]
compB (Not exp) = (compB exp) ++ [Neg]
compB (And lhs rhs) = (compB rhs) ++ (compB lhs) ++ [And]
compB (BEq lhs rhs) = (compB rhs) ++ (compB lhs) ++ [Equ]
compB (IEq lhs rhs) = (compA rhs) ++ (compA lhs) ++ [Equ]
