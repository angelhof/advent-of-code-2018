module Day1 where
import qualified Data.Set as Set

main :: IO ()
main = do
  lines <- readLines "input/day1.in"
  print $ foldl addInteger 0 lines
  print $ firstDupFreq (0, Set.singleton 0) $ cycle lines

readLines :: String -> IO [String]
readLines filename = do
  content <- readFile filename
  return $ lines content

addInteger :: Integer -> String -> Integer
addInteger acc (sign:number) = eval acc sign $ read number

-- This can be generalized on a conditional fold on streams
firstDupFreq :: (Integer, Set.Set Integer) -> [String] -> Integer
firstDupFreq (acc, set) ((sign:number):rest) =
  let acc' = eval acc sign (read number) in
   if Set.member acc' set
   then acc'
   else firstDupFreq (acc', Set.insert acc' set) rest

eval :: Integer -> Char -> Integer -> Integer
eval x '+' y = x + y
eval x '-' y = x - y
