module VM where

-- the programming language's values
import Utils.Value ( Value(..) )
import qualified Utils.Value as Value

-- the bytecode instructions of the programming language
import Utils.Inst ( Inst(..), Code )

-- the machine's stack
import qualified Utils.Stack as Stack (push)
import Utils.Stack (Stack, createEmptyStack, stack2Str)

-- the machine's state
import qualified Utils.State as State (push, find)
import Utils.State (State, createEmptyState, state2Str)

-- Reusable function that pops a value from the stack and performs a unary operation with it.
-- The result of the operation is then pushed to the top of the stack.
applyUnaryOp :: (Value -> Maybe Value) -> Stack -> Stack
applyUnaryOp op ( x:stack ) =
  case op x of
    Nothing -> error "Run-time error"
    Just value -> Stack.push value stack
applyUnaryOp _ _ = error "Run-time error"

-- Reusable function that pops two values from the stack and performs a binary operation with them.
-- The result of the operation is then pushed to the top of the stack.
applyBinaryOp :: (Value -> Value -> Maybe Value) -> Stack -> Stack
applyBinaryOp op ( lhs:rhs:stack ) =
  case op lhs rhs of
    Nothing -> error "Run-time error"
    Just value -> Stack.push value stack
applyBinaryOp _ _ = error "Run-time error"

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)

-- Dummy instruction
run ( (Noop):xs, stack, state) = run (xs, stack, state)

-- Push an integer to the stack.
run ( (Push i):xs, stack, state) =
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
    Just value -> run ( xs, Stack.push value stack, state )
    Nothing -> error "Run-time error"

-- Fetches the current value of a variable and pushes it to the top of the stack.
run ( (Store s):xs, value:stack, state ) =
  run ( xs, stack, State.push s value state )
run ( (Store s):_, [], _ ) = error "Run-time error"

-- A conditional branch.
run ( (Branch c1 c2):xs, (B True):stack, state ) =
  run ( c1 ++ xs, stack, state )
run ( (Branch c1 c2):xs, (B False):stack, state ) =
  run ( c2 ++ xs, stack, state )
run ( (Branch _ _):_, _, _ ) = error "Run-time error"

-- A loop.
run ( (Loop cond code):xs, stack, state ) =
  case run ( cond, stack, state) of
    ( [], (B True):stack', state' ) -> run ( (code ++ [Loop cond code] ++ xs), stack', state' )
    ( [], (B False):stack', state' ) -> run ( xs, stack', state' )
    (_, _, _) -> error "Run-time error"
