module Day8 where

import Lib
import Data.Maybe as M

main :: IO ()
main = do
  [line] <- readLines "input/day8.in"
  let (tree, []) = parseTree $ map read $ words line in do
   print $ foldTree sumMetadata 0 tree
   print $ value tree

-- Returns the value of a node
value :: Tree -> Int
value (Node [] metadata) = sum metadata
value (Node children metadata) = sum $ [M.fromJust v | v <- indexed, M.isJust v]
  where
    values = map value children
    indexed = map ((nth values).(\x -> x - 1)) metadata
  
-- Function that sums the metadata entry of a node
sumMetadata :: Int -> Tree -> Int
sumMetadata acc (Node _ metadata) = acc + sum metadata

-- Generic fold over a tree                                        
foldTree :: (a -> Tree -> a) -> a -> Tree -> a
foldTree f acc t@(Node children _) = f acc' t
  where
    acc' = foldl (foldTree f) acc children
  
--
-- Parsing the input
--

type Token = Int
  
data Tree = Node [Tree] [Int]
            deriving (Show)

parseTree :: [Token] -> (Tree, [Token])
parseTree (nC:nM:rest) = (Node children metadata, rest'')
  where
    (children, rest') = parseChildren nC rest
    (metadata, rest'') = parseMetadata nM rest'

parseChildren :: Int -> [Token] -> ([Tree], [Token])
parseChildren nC tokens = foldl parseChildren' ([], tokens) [1..nC]
  where
    parseChildren' (trees, tokens) _ = (trees ++ [tree], rest)
      where
        (tree, rest) = parseTree tokens

parseMetadata :: Int -> [Token] -> ([Int], [Token])
parseMetadata = splitAt
