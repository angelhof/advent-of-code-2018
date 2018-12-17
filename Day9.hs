module Day9 where

import Lib
import qualified Data.Map as M

inputPlayers = 455
inputMarbles = 71223

-- Note: It has to be executed with a bigger than default
--       stack size for the second part of the problem
main :: IO ()
main = do
  let circle = initCircle inputPlayers in do
  print . maxScore $ playRounds inputMarbles circle;
  print . maxScore $ playRounds (inputMarbles*100) circle
  
initCircle players = (([], 0, []), 1, (players, M.fromList [(p,0) | p <- [0..(players-1)]]))

type Marbles = ([Int], Int, [Int])
type Scores = (Int, M.Map Int Int)
type Circle = (Marbles, Int, Scores)

maxScore :: Circle -> Int
maxScore (_, _, (_, scores)) = maximum $ map snd $ M.toList scores

playRounds :: Int -> Circle -> Circle
playRounds n circle = foldr (\_ -> playRound) circle [1..n]

playRound :: Circle -> Circle
playRound c@(marbles, next, scores)
  | next `mod` 23 /= 0 = (insertMarble next $ goForward marbles, next + 1, scores)
  | otherwise = (marbles', next + 1, scores')
  where
    (marbles', removed) = removeMarble $ goBackwards 7 marbles
    scores' = updateScores next (next + removed) scores    
    
updateScores :: Int -> Int -> Scores -> Scores
updateScores round newScore (players, scores) = (players, M.adjust (newScore +) player scores)
  where
    player = round `mod` players


-- Zippers on cycling lists
goBackwards :: Int -> Marbles -> Marbles
goBackwards n marbles = foldr (\_ -> goBackward) marbles [1..n]

goForward :: Marbles -> Marbles
goForward ([], m, []) = ([], m, [])
goForward ([], m, bs) = goForward (reverse bs, m, [])
goForward (a:as, m, bs) = (as, a, m:bs)

goBackward :: Marbles -> Marbles
goBackward ([], m, []) = ([], m, [])
goBackward (as, m, []) = goBackward ([], m, reverse as)
goBackward (as, m, b:bs) = (m:as, b, bs)

insertMarble :: Int -> Marbles-> Marbles
insertMarble x (as, m, bs) = (as, x, m:bs)

removeMarble :: Marbles -> (Marbles, Int)
removeMarble ([], m, b:bs) = (([], b, bs), m)
removeMarble (a:as, m, bs) = ((as, a, bs), m)
