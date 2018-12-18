module Lib where

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

-- This function reads a file and returns its lines.
-- It doesn't do any error handling if the file doesn't exist
readLines :: String -> IO [String]
readLines filename = do
  content <- readFile filename
  return $ lines content

--
-- Tree Functions
--


data Tree a = Node a [Tree a]
            deriving (Show)

-- Generic map over a tree
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node x children) = Node (f x) children'
  where
    children' = map (mapTree f) children

-- Generic fold over a tree                                        
foldTree :: (a -> Tree b -> a) -> a -> Tree b -> a
foldTree f acc t@(Node _ children) = f acc' t
  where
    acc' = foldl (foldTree f) acc children  

-- This is a generic bottom up fold function
foldUp :: ([b] -> a -> b) -> b -> Tree a -> b
foldUp f acc (Node x children) = f (map (foldUp f acc) children) x

-- It checks whether the second element of a tuple is equal to x
sndEq :: Eq b => b -> (a, b) -> Bool
sndEq x (_, snd) = snd == x

-- Converts a boolean to an Integer
boolToInt :: Bool -> Integer
boolToInt True = 1
boolToInt False = 0

-- It extracts the third element of a triple
second (_, s, _) = s
third (_, _, t) = t

-- Update the snd element of a tuple
uSnd f (a, b) = (a, f b)

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

-- This functions filters and keeps all the Just from a list
extractJust :: [Maybe a] -> [a]
extractJust = (map Maybe.fromJust) . (filter Maybe.isJust)

-- This returns an integer from its reverse digits
fromDigits :: [Int] -> Int
fromDigits = fromDigits' 1
  where
  fromDigits' acc [] = 0
  fromDigits' acc (d:ds) = d*acc + fromDigits' (acc * 10) ds

-- This returns the digits of an integer
toDigits :: Int -> [Int]
toDigits x =
  let
    digs 0 = []
    digs x = x `mod` 10 : digs (x `div` 10)
  in
  case digs x of
   [] -> [0]
   xDigs -> xDigs

-- Int to string with leading zeros
showD :: Int -> Int -> String 
showD d n = take (d - (length str)) (repeat '0') ++ str
  where str = show n

-- Compose of a 2 argument function with a one argument function
inj :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `inj` op = \x y -> f (op x) (op y)

-- Apply a function on a map and an accumulator until the map is empty
foldMapGeneric :: (Map.Map k a -> b -> (Map.Map k a, b)) -> Map.Map k a -> b -> b
foldMapGeneric f m acc
  | Map.null m  = acc
  | otherwise = foldMapGeneric f m' acc'
  where
    (m', acc') = f m acc
