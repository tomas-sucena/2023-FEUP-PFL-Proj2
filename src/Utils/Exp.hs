module Utils.Exp where

-- arithmetic expressions
data Aexp =
  Int Integer         -- constants
  | Add Aexp Aexp     -- addition
  | Mult Aexp Aexp    -- multiplication
  | Sub Aexp Aexp   -- subtraction

-- boolean expressions
data Bexp =
  Bool Bool           -- constants
  | Not Bexp          -- negation
  | And Bexp Bexp     -- logical AND
  | Eq Bexp Bexp      -- boolean equality
  | IntEq Aexp Aexp   -- integer equality

-- statements
data Stm =
  Assign String Aexp -- integer assignment
  | If Bexp Stm -- if
  | While Bexp Stm -- while
  | Until Bexp Stm -- until
