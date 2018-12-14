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
value :: MTree -> Int
value (Node metadata []) = sum metadata
value (Node metadata children) = sum $ [M.fromJust v | v <- indexed, M.isJust v]
  where
    values = map value children
    indexed = map ((nth values).(\x -> x - 1)) metadata
    
-- Function that sums the metadata entry of a node
sumMetadata :: Int -> MTree -> Int
sumMetadata acc (Node metadata _) = acc + sum metadata
  
--
-- Parsing the input
--

type Token = Int
  
type MTree = Tree [Int]

parseTree :: [Token] -> (MTree, [Token])
parseTree (nC:nM:rest) = (Node metadata children, rest'')
  where
    (children, rest') = parseChildren nC rest
    (metadata, rest'') = parseMetadata nM rest'

parseChildren :: Int -> [Token] -> ([MTree], [Token])
parseChildren nC tokens = foldl parseChildren' ([], tokens) [1..nC]
  where
    parseChildren' (trees, tokens) _ = (trees ++ [tree], rest)
      where
        (tree, rest) = parseTree tokens

parseMetadata :: Int -> [Token] -> ([Int], [Token])
parseMetadata = splitAt
