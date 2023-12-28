module Utils.State where

import qualified Data.Map as Map -- tree maps
import Utils.Value

type State = Map.Map String Value

-- Creates an empty machine state.
createEmptyState :: State
createEmptyState = Map.empty

-- Inserts a variable and its value in the machine's state.
-- If the variable is already present, its value is replaced with the new one.
push :: String -> Value -> State -> State
push key value state = Map.insert key value state

-- Prints the values on the machine's state.
state2Str :: State -> String
state2Str state
  | acc == "" = ""
  | otherwise = init acc -- return everything except the last comma
  where (acc, _) = Map.mapAccumWithKey printVar "" state
        printVar acc key value = (acc ++ key ++ "=" ++ show value ++ ",", Nothing)