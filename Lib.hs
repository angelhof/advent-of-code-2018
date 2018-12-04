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
