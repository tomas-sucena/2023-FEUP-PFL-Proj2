module Utils.Stm where

-- arithmetic expressions
data Aexp =
  I Integer           -- constant
  | Var String        -- variables
  | Add Aexp Aexp     -- addition
  | Mult Aexp Aexp    -- multiplication
  | Sub Aexp Aexp     -- subtraction
  deriving Show

-- boolean expressions
data Bexp =
  B Bool              -- constants
  | Le Aexp Aexp      -- integer less or equal
  | IEqu Aexp Aexp    -- integer equality
  | Not Bexp          -- negation
  | BEqu Bexp Bexp    -- boolean equality
  | And Bexp Bexp     -- logical AND
  | Or Bexp Bexp      -- logical OR
  deriving Show

-- statements
data Stm =
  Assign String Aexp -- integer assignment
  | If Bexp [Stm] [Stm] -- if
  | While Bexp [Stm] -- while
  deriving Show

type Program = [Stm]
