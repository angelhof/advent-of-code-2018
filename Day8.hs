module Day8 where

import Lib

main :: IO ()
main = do
  [line] <- readLines "input/day8.in"
  let (tree, []) = parseTree $ map read $ words line in do
   print $ foldTree sumMetadata 0 tree
   print $ foldUp value 0 tree

type Metadata = [Int]
type MTree = Tree Metadata
  
-- This is to be used with foldUp which folds a tree bottom up
value :: [Int] -> Metadata -> Int
value [] = sum 
value values = sum . extractJust . (map ((nth values).(\x -> x - 1)))
                                 
-- Function that sums the metadata entry of a node
sumMetadata :: Int -> MTree -> Int
sumMetadata acc (Node metadata _) = acc + sum metadata
  
--
-- Parsing the input
--

type Token = Int

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
