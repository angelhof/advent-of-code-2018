module Day14 where

import Lib
import Data.List
import qualified Data.Map as M

input1 :: Int
input1 = 360781

input2 :: [Int]
-- input2 = map (\c -> read [c]) "59414"
input2 = map (\c -> read [c]) "360781"

-- WARNING: This is pretty naive and takes a long time to execute
main :: IO ()
main = do
  let recipes1 = second $ execUntil (lenGT (input1 + 10)) initState in
   let last10 = getRecipes [input1..(input1+9)] recipes1 in
    let (_, recipes2, len2) =  execUntil (infixIn input2) initState in do
   print . (showD 10) . fromDigits . reverse $ last10
   print len2
   print . (showD 10) . fromDigits . reverse $ getRecipes [(max 0 (len2-10))..(len2-1)] recipes2

type State = ((Int, Int), M.Map Int Int, Int)

initState :: State
initState = ((0,1), M.fromList [(0, 3), (1, 7)], 2)


-- Those 2 functions are used as termination predicates
lenGT :: Int -> State -> Bool
lenGT n (_, _, len) = len >= n

-- There is an implicit assumption here that only 2 recipes could be added each round
infixIn :: [Int] -> State -> Bool
infixIn key (_, recipes, len) = isInfixOf key $ getRecipes [(max 0 (len-10))..(len-1)] recipes

execUntil :: (State -> Bool) -> State -> State
execUntil f state = if f state then state else execUntil f $ execRound state

execRound :: State -> State
execRound = updateIndexes . addRecipes

addRecipes :: State -> State
addRecipes state@((i1, i2), recipes, len) = appendRecipes newRecipes state
  where
    newRecipes = toDigits $ (getRecipe i1 recipes) + (getRecipe i2 recipes) 

updateIndexes :: State -> State
updateIndexes ((i1, i2), recipes, len) = ((i1', i2'), recipes, len)
  where
    i1' = (i1 + 1 + (getRecipe i1 recipes)) `mod` len
    i2' = (i2 + 1 + (getRecipe i2 recipes)) `mod` len    


--
-- Interface functions to get and add recipes to the recipes data structure
--

getRecipes :: [Int] -> M.Map Int Int -> [Int] 
getRecipes is recipes = map (\x -> getRecipe x recipes) is
              
getRecipe :: Int -> M.Map Int Int -> Int
getRecipe i recipes = recipes M.! i

appendRecipes :: [Int] -> State -> State
appendRecipes newRecipes state  = foldr appendRecipe state newRecipes

appendRecipe :: Int -> State -> State
appendRecipe newR (is, recipes, len) = (is, M.insert len newR recipes, len + 1)

