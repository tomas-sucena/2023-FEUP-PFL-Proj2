module VM where

-- the language's values
import Utils.Value ( Value(I, B) )

-- the machine's stack
import qualified Utils.Stack as Stack (push, pop, top)
import Utils.Stack (createEmptyStack, stack2Str)

-- the machine's state
import qualified Utils.State as State (push)
import Utils.State (createEmptyState, state2Str)
