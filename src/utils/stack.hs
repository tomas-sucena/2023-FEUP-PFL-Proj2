
data Value =
  I Integer | Tt | Ff

instance Show Value where
  show (I i) = show i
  show Tt = show True
  show Ff = show False

type Stack = [Value]

-- Pushes a value onto the stack.
push :: Value -> Stack -> Stack
push el [] = el:[]
push el s = el:s

-- Pops the value on top of the stack.
pop :: Stack -> Stack
pop [] = error "Run-time error"
pop (x:xs) = xs

-- Returns the value on top of the stack.
top :: Stack -> Value
top [] = error "Run-time error"
top (x:xs) = x

-- Prints the values on the stack.
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str (x:[]) = show x
stack2Str (x:xs) = show x ++ "," ++ (stack2Str xs)
