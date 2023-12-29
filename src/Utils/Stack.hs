module Utils.Stack where

import Data.List (intercalate)
import Utils.Value

type Stack = [Value]

-- Creates a new empty stack.
createEmptyStack :: Stack
createEmptyStack = []

-- Pushes a value onto the stack.
push :: Value -> Stack -> Stack
push el [] = el:[]
push el stack = el:stack

-- Pops the value on top of the stack.
-- Returns the value that was popped and the updated stack.
pop :: Stack -> (Maybe Value, Stack)
pop [] = (Nothing, [])
pop (x:xs) = (Just x, xs)

-- Prints the values on the stack.
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map show stack)
