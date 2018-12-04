module Day2 where

import Lib
import qualified Data.Map as Map

main :: IO ()
main = do
  lines <- readLines "input/day2.in"
  print $ checksum lines
  print $ findPairSimilar lines

checksum :: [String] -> Integer
checksum lines = sum r2 * sum r3
  where
    charOccs = map (Map.toList . (Map.fromListWith (+)) . (map (\c -> (c,1)))) lines
    (r2, r3) = unzip $ map twoThreeDups charOccs
    
twoThreeDups :: [(Char, Integer)] -> (Integer, Integer)
twoThreeDups occs = (boolToInt $ any (sndEq 2) occs, boolToInt $ any (sndEq 3) occs)

-- Naive. Assuming that there exists a pair of similar.
findPairSimilar :: [String] -> String
findPairSimilar (x:xs) =
  case findSimilar x xs of
   Nothing -> findPairSimilar xs
   Just y -> fst $ unzip $ filter (\(c1,c2) -> c1 == c2) $ zip x y
  
findSimilar :: String -> [String] -> Maybe String
findSimilar x [] = Nothing
findSimilar x (y:ys) =
  if isSimilar x y
  then Just y
  else findSimilar x ys
  
isSimilar :: String -> String -> Bool
isSimilar x y = ((toInteger . length) x - 1) == (sum $ map (\(c1,c2) -> boolToInt (c1 == c2)) $ zip x y)
    

