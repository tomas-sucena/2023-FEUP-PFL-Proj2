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
  B Bool           -- constants
  | Not Bexp          -- negation
  | And Bexp Bexp     -- logical AND
  | BEqu Bexp Bexp    -- boolean equality
  | IEqu Aexp Aexp    -- integer equality
  | Le Aexp Aexp      -- integer less or equal
  deriving Show

-- statements
data Stm =
  Assign String Aexp -- integer assignment
  | If Bexp [Stm] [Stm] -- if
  | While Bexp [Stm] -- while
  deriving Show

type Program = [Stm]
