module Compiler where

import Utils.Exp ( Aexp(..), Bexp(..), Stm(..) )
import Utils.Inst ( Inst(..), Code )

-- Compiles an arithmetic expression
compA :: Aexp -> Code
compA (Int i) = [Push i]
compA (Add lhs rhs) = (compA lhs) ++ (compA rhs) ++ [Add]
compA (Mult lhs rhs) = (compA lhs) ++ (compA rhs) ++ [Mult]
compA (Sub lhs rhs) = (compA lhs) ++ (compA rhs) ++ [Sub]
