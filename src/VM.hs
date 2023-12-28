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
import qualified Utils.State as State (push)
import Utils.State (State, createEmptyState, state2Str)

applyUnaryOp :: (Value -> Value) -> Stack -> Stack
applyUnaryOp op ( x:stack ) = (op x):stack
applyUnaryOp _ _ = error "Run-time error!"

applyBinaryOp :: (Value -> Value -> Value) -> Stack -> Stack
applyBinaryOp op ( x:y:stack ) = (op x y):stack
applyBinaryOp _ _ = error "Run-time error!"

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push i):xs, stack, state) =
  run (xs, Stack.push (I i) stack, state) -- Push an integer to the stack.
run ( Add:xs, stack, state ) =
  run ( xs, applyBinaryOp (Value.+) stack, state ) -- Add two integers.
run ( Mult:xs, stack, state ) =
  run ( xs, applyBinaryOp (Value.*) stack, state ) -- Multiply two integers.
run ( Sub:xs, stack, state ) =
  run ( xs, applyBinaryOp (Value.-) stack, state ) -- Subtract two integers.
run (Tru:xs, stack, state) =
  run (xs, Stack.push (B True) stack, state) -- Push 'True' to the stack.
run (Fals:xs, stack, state) =
  run (xs, Stack.push (B False) stack, state) -- Push 'False' to the stack.
