module Utils.Inst where

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer          -- push an integer value to the stack
  | Add                 -- add two integers and push the result to the stack
  | Mult                -- multiply two integers and push the result to the stack
  | Sub                 -- subtract two integers and push the result to the stack
  | Tru                 -- push 'True' to the stack
  | Fals                -- push 'False' to the stack
  | Equ                 -- test if two values are equal and push the result to the stack
  | Le                  -- test if an integer is less or equal than another and push the result to the stack
  | And                 -- perform a logical AND between two booleans and push the result to the stack
  | Neg                 -- negate a boolean and push the result to the stack
  | Fetch String        -- fetch the value of a variable and push it to the stack
  | Store String        -- pop the value on top of the stack and store it in a variable
  | Noop                -- do nothing
  | Branch Code Code    -- conditional branch
  | Loop Code Code      -- loop
  deriving Show

type Code = [Inst]
