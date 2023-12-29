module Utils.Stm where

-- arithmetic expressions
data Aexp =
  Int Integer         -- constants
  | Var String        -- variables
  | Add Aexp Aexp     -- addition
  | Mult Aexp Aexp    -- multiplication
  | Sub Aexp Aexp     -- subtraction
  deriving Show

-- boolean expressions
data Bexp =
  Bool Bool           -- constants
  | Not Bexp          -- negation
  | And Bexp Bexp     -- logical AND
  | BEq Bexp Bexp     -- boolean equality
  | IEq Aexp Aexp     -- integer equality
  deriving Show

-- statements
data Stm =
  Assign String Aexp -- integer assignment
  | If Bexp Stm -- if
  | While Bexp Stm -- while
  deriving Show

type Program = [Stm]
