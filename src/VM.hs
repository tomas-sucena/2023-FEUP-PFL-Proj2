module VM where

-- the programming language's values
import Utils.Value ( Value(..) )
import qualified Utils.Value as Value

-- the bytecode instructions of the programming language
import Utils.Inst ( Inst(..), Code )

-- the machine's stack
import qualified Utils.Stack as Stack (push, pop)
import Utils.Stack (Stack, createEmptyStack, stack2Str)

-- the machine's state
import qualified Utils.State as State (push, find)
import Utils.State (State, createEmptyState, state2Str)

applyUnaryOp :: (Value -> Value) -> Stack -> Stack
applyUnaryOp op ( x:stack ) = (op x):stack
applyUnaryOp _ _ = error "Run-time error"

applyBinaryOp :: (Value -> Value -> Value) -> Stack -> Stack
applyBinaryOp op ( x:y:stack ) = (op x y):stack
applyBinaryOp _ _ = error "Run-time error"

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)

-- Dummy instruction
run ( (Noop):xs , stack, state) -> (xs, stack, state)

 -- Push an integer to the stack.
run ((Push i):xs, stack, state) =
  run (xs, Stack.push (I i) stack, state)

-- Add two integers.
run ( Add:xs, stack, state ) =
  run ( xs, applyBinaryOp (Value.+) stack, state )

-- Multiply two integers.
run ( Mult:xs, stack, state ) =
  run ( xs, applyBinaryOp (Value.*) stack, state )

-- Subtract two integers.
run ( Sub:xs, stack, state ) =
  run ( xs, applyBinaryOp (Value.-) stack, state )

-- Push 'True' to the stack.
run ( Tru:xs, stack, state ) =
  run (xs, Stack.push (B True) stack, state )

-- Push 'False' to the stack.
run ( Fals:xs, stack, state ) =
  run (xs, Stack.push (B False) stack, state )

-- Compare two values for equality.
run ( Equ:xs, stack, state ) =
  run (xs, applyBinaryOp (Value.==) stack, state)

-- Verify if an integer is less or equal than another.
run ( Le:xs, stack, state ) =
  run (xs, applyBinaryOp (Value.<=) stack, state)

-- Applies the logical AND operator to a pair of bools.
run ( And:xs, stack, state ) =
  run (xs, applyBinaryOp (Value.&&) stack, state)

-- Negates a bool.
run ( Neg:xs, stack, state ) =
  run (xs, applyUnaryOp (Value.not) stack, state)

-- Fetches the current value of a variable and pushes it to the top of the stack.
run ( (Fetch s):xs, stack, state ) =
  case State.find s state of
    Just value -> ( xs, Stack.push value stack, state )
    Nothing -> error "Run-time error"

-- Fetches the current value of a variable and pushes it to the top of the stack.
run ( (Store s):xs, value:stack, state ) =
  ( xs, stack, State.push s value state )
run ( (Store s):_, [], _ ) = error "Run-time error"
