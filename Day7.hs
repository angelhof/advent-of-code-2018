module Day7 where

import Lib
import Data.List
import Data.Char as C
import qualified Data.Map as M

main :: IO ()
main = do
  lines <- readLines "input/day7.in"
  let edges = edgeMap $ map parseEdge lines in do
    print . totalOrder $ edges;
    print . last $ totalWork 5 edges

type V = Char
type E = (Char, Char)
type EdgeMap = M.Map V [V] 
type CostEdgeMap = M.Map V (Int, [V])

-- Part 2

totalWork :: Int -> EdgeMap -> [Int]
totalWork workers edges =
  foldMapGeneric removeAndAllocate (M.map (\vs -> (0, vs)) edges) (map (\_ -> 0) [1..workers])

addWork :: [Int] -> (V, Int) -> ([Int], Int)
addWork (w:ws) (v, startTime) = (sort $ endTime:ws, endTime)
  where
    endTime = (max startTime w) + cost v

cost :: V -> Int
cost v = C.ord v - 4

removeAndAllocate :: CostEdgeMap -> [Int] -> (CostEdgeMap, [Int])
removeAndAllocate edges workers = (edges', workers')
  where
    first = findFirstToAllocate edges
    (workers', earliestStartTime) = addWork workers first
    edges' = removeAndUpdateNode (fst first, earliestStartTime) edges

findFirstToAllocate :: CostEdgeMap -> (V, Int)
findFirstToAllocate =
  (minimumBy (compare `inj` snd)) . (map (\(v, (t, _)) -> (v, t))) . (filter (null . snd . snd)) . M.toList 

removeAndUpdateNode :: (V, Int) -> CostEdgeMap -> CostEdgeMap
removeAndUpdateNode (v, earliest) edges = M.map removeAndUpdateNode' $ M.delete v edges
  where
    removeAndUpdateNode' n@(startTime, incoming) =
      if v `elem` incoming then (max earliest startTime, delete v incoming) else n

-- Part 1
                                          
totalOrder :: EdgeMap -> [V]
totalOrder edges = reverse $ foldMapGeneric totalOrder' edges []
  where
    totalOrder' edges acc = uSnd (\v -> v:acc) $ removeFirst edges

removeFirst :: EdgeMap -> (EdgeMap, V)
removeFirst edges = (edges', first)
  where
    first = findFirst edges
    edges' = removeNode first edges

findFirst :: EdgeMap -> V
findFirst edges = minimum . fst . unzip $ filter (null . snd) $ M.toList edges

removeNode :: V -> EdgeMap -> EdgeMap
removeNode v edges = M.map (delete v) $ M.delete v edges

edgeMap :: [E] -> EdgeMap
edgeMap edges = foldr addIncomingEdge emptyMap edges
  where
    (vs1, vs2) = unzip edges
    emptyMap = M.fromList [(v, []) | v <- vs1 ++ vs2]

addIncomingEdge :: E -> EdgeMap -> EdgeMap
addIncomingEdge (v1,v2) edges = M.adjust (v1:) v2 edges

-- Parsing the input
parseEdge :: String -> E
parseEdge line = (v1, v2)
  where
    [_, [v1], _, _, _, _, _, [v2], _, _] = words line
