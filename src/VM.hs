module VM where

-- the programming language's values
import Utils.Value ( Value(..) )

-- the bytecode instructions of the programming language
import Utils.Inst ( Inst(..), Code )

-- the machine's stack
import qualified Utils.Stack as Stack (push, pop, top)
import Utils.Stack (Stack, createEmptyStack, stack2Str)

-- the machine's state
import qualified Utils.State as State (push)
import Utils.State (State, createEmptyState, state2Str)

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run ((Push i):xs, stack, state) = run (xs, Stack.push (I i) stack, state)
run (Tru:xs, stack, state) = run (xs, Stack.push (B True) stack, state)
run (Fals:xs, stack, state) = run (xs, Stack.push (B False) stack, state)
