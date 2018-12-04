module Day3 where

import Lib
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set

main :: IO ()
main = do
  lines <- readLines "input/day3.in"
  let rects = map parseLine lines in do
   print . length . overlappingRids $ rects
   print . nonoverlappingRectId $ rects

-- Removes all the rids of overlapping rectangulars,
-- ending up with the rid of the only rect that doesn't overlap
nonoverlappingRectId :: [Rect] -> Int
nonoverlappingRectId rects = uniqueRid
  where
    oRids = overlappingRids rects
    rids = Set.fromList [ rid | Rect rid _ _ _ _ <- rects ]
    [uniqueRid] = Set.toList $ foldr Set.delete rids $ concat oRids

-- Returns a list of the lists of all overlaping rids for each point
-- where there is overlap.
overlappingRids :: [Rect] -> [[Int]]
overlappingRids = (filter (\l -> 1 < length l)) . Map.elems . paintedCanvas

--
-- Painting the canvas with rectangulars
-- 

paintedCanvas :: [Rect] -> Canvas
paintedCanvas = foldl paintRect Map.empty

paintRect :: Canvas -> Rect -> Canvas
paintRect canvas (Rect rid x0 y0 x1 y1) =
  foldl paintPoint canvas [(rid, (x,y)) | x <- [x0..x1], y <- [y0..y1]]

paintPoint :: Canvas -> (Int, (Int, Int)) -> Canvas
paintPoint canvas (rid, point) = Map.alter paintPoint' point canvas
  where
    paintPoint' Nothing = Just [rid]
    paintPoint' (Just rids) = Just $ (rid:rids)

--
-- Parsing the input
--
  
data Rect = Rect Int Int Int Int Int
          deriving (Show)

type Canvas = Map.Map (Int, Int) [Int]

parseLine :: String -> Rect
parseLine line = Rect (read rid) ix0 iy0 (ix0 - 1 + read dx) (iy0 - 1 + read dy) 
  where
    ix0 = read x0
    iy0 = read y0
    [('#':rid), "@", x0y0, dxdy] = words line
    [x0, y0] = splitOn "," $ init x0y0
    [dx, dy] = splitOn "x" dxdy
