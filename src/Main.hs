import Utils.Stack (createEmptyStack, stack2Str)
import Utils.State (createEmptyState, state2Str)
import Utils.Inst ( Inst(..), Code )

import Parser (parse)       -- steps 1 and 2: lex and parse the input
import Compiler (compile)   -- step 3: compile the statements into instructions
import VM (run)             -- step 4: run the instructions on the virtual machine

-- Auxiliary function defined by the teachers to help us test our virtual machine.
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

-- Auxiliary function defined by the teachers to help us test our parser.
-- In reality, this function tests the whole compilation process.
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)
