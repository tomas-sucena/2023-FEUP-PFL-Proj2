module Utils.Value where

data Value =
  I Integer | B Bool

instance Show Value where
  show (I i) = show i
  show (B b) = show b

-- operator overloading
(+) :: Value -> Value -> Maybe Value
(+) (I lhs) (I rhs) = Just (I (lhs Prelude.+ rhs) )
(+) _ _ = Nothing

(*) :: Value -> Value -> Maybe Value
(*) (I lhs) (I rhs) = Just (I (lhs Prelude.* rhs) )
(*) _ _ = Nothing

(-) :: Value -> Value -> Maybe Value
(-) (I lhs) (I rhs) = Just (I (lhs Prelude.- rhs) )
(-) _ _ = Nothing

(==) :: Value -> Value -> Maybe Value
(==) (I lhs) (I rhs) = Just (B (lhs Prelude.== rhs) )
(==) (B lhs) (B rhs) = Just (B (lhs Prelude.== rhs) )
(==) _ _ = Nothing

(<=) :: Value -> Value -> Maybe Value
(<=) (I lhs) (I rhs) = Just (B (lhs Prelude.<= rhs) )
(<=) _ _ = Nothing

(&&) :: Value -> Value -> Maybe Value
(&&) (B lhs) (B rhs) = Just (B (lhs Prelude.&& rhs) )
(&&) _ _ = Nothing

not :: Value -> Maybe Value
not (B x) = Just (B (Prelude.not x) )
not _ = Nothing
