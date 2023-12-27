module Utils.Stack where
import Utils.Value

type Stack = [Value]

-- Pushes a value onto the stack.
push :: Value -> Stack -> Stack
push el [] = el:[]
push el s = el:s

-- Pops the value on top of the stack.
pop :: Stack -> Maybe Stack
pop [] = Nothing
pop (x:xs) = Just xs

-- Returns the value on top of the stack.
top :: Stack -> Maybe Value
top [] = Nothing
top (x:xs) = Just x

-- Prints the values on the stack.
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (x:[]) = show x
stack2Str (x:xs) = show x ++ "," ++ (stack2Str xs)

-- Creates a new empty stack.
createEmptyStack :: Stack
createEmptyStack = []
