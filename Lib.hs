module Lib where

-- This function reads a file and returns its lines.
-- It doesn't do any error handling if the file doesn't exist
readLines :: String -> IO [String]
readLines filename = do
  content <- readFile filename
  return $ lines content

-- It checks whether the second element of a tuple is equal to x
sndEq :: Eq b => b -> (a, b) -> Bool
sndEq x (_, snd) = snd == x

-- Converts a boolean to an Integer
boolToInt :: Bool -> Integer
boolToInt True = 1
boolToInt False = 0

-- It extracts the third element of a triple
third (_, _, t) = t

-- This function orders to elements by using a getter
-- Note: This is a more general composition of a binary
--       function with a unary function
ordGet :: Ord a => (b -> a) -> b -> b -> Ordering
ordGet getter x1 x2 = compare (getter x1) (getter x2)

-- An 0-indexing function returning maybe and an element
nth :: [a] -> Int -> Maybe a
nth list i
  | i < 0 = Nothing 
  | otherwise =
    case drop i list of
     [] -> Nothing
     (x:_) -> Just x
