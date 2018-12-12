module Day5 where

import Lib
import Data.List
import qualified Data.Char as Char

main :: IO ()
main = do
  [line] <- readLines "input/day5.in"
  print . length $ reactFixpoint line
  print . minimum $ map (length . reactFixpoint . removeUnit line) ['a'..'z']

-- Removes all instances of a unit from the polymer
removeUnit :: String -> Char  -> String
removeUnit polymer c = filter (\x -> x /= c && x /= Char.toUpper c) polymer

-- Reacts until it is no longer possible
reactFixpoint :: String -> String
reactFixpoint polymer =
  let polymer' = reactPolymer polymer in
  if polymer' == polymer
  then polymer'
  else reactFixpoint polymer'

-- Runs the reactions on a polymer once
reactPolymer :: String -> String
reactPolymer = foldr react ""

react :: Char -> String -> String
react x [] = [x]
react x (c:cs)
  | areOpposite x c = cs
  | otherwise = x:c:cs

areOpposite :: Char -> Char -> Bool
areOpposite c1 c2 = (Char.toLower c1 == Char.toLower c2) && (c1 /= c2)
