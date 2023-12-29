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
run ( (Loop c1 c2):xs, stack, state ) =
  case run ( c1, stack, state) of
    ( [], (B True):stack', state' ) -> run ( (c2 ++ [Loop c1 c2] ++ xs), stack', state' )
    ( [], (B False):stack', state' ) -> run ( xs, stack', state' )
    (_, _, _) -> error "Run-time error"

-- Auxiliary function defined by the teachers to help us test our virtual machine.
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
-- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
-- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
-- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
-- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
-- testAssembler [Push (-20),Push (-21), Le] == ("True","")
-- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
-- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
-- If you test:
-- testAssembler [Push 1,Push 2,And]
-- You should get an exception with the string: "Run-time error"
-- If you test:
-- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
-- You should get an exception with the string: "Run-time error"
