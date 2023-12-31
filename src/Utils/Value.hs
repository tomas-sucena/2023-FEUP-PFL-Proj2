module Utils.Value where

data Value =
  I Integer | B Bool

instance Show Value where
  show (I i) = show i
  show (B b) = show b

-- operator overloading
(+) :: Value -> Value -> Value
(+) (I lhs) (I rhs) = (I (lhs Prelude.+ rhs) )
(+) _ _ = error "Run-time error"

(*) :: Value -> Value -> Value
(*) (I lhs) (I rhs) = (I (lhs Prelude.* rhs) )
(*) _ _ = error "Run-time error"

(-) :: Value -> Value -> Value
(-) (I lhs) (I rhs) = (I (lhs Prelude.- rhs) )
(-) _ _ = error "Run-time error"

(==) :: Value -> Value -> Value
(==) (I lhs) (I rhs) = (B (lhs Prelude.== rhs) )
(==) (B lhs) (B rhs) = (B (lhs Prelude.== rhs) )
(==) _ _ = error "Run-time error"

(<=) :: Value -> Value -> Value
(<=) (I lhs) (I rhs) = (B (lhs Prelude.<= rhs) )
(<=) _ _ = error "Run-time error"

(&&) :: Value -> Value -> Value
(&&) (B lhs) (B rhs) = (B (lhs Prelude.&& rhs) )
(&&) _ _ = error "Run-time error"

not :: Value -> Value
not (B x) = (B (Prelude.not x) )
not _ = error "Run-time error"
