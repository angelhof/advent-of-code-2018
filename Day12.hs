module Main where

import Lib
import Data.List
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS
import qualified Data.Map as M



-- TODO: I need to reimplement it without all those disgusting bitstrings and
--       check for a repeating pattern. Whenever I find one, I should just
--       keep a list of the repeating patterns and then execute the rest of the
--       executions by just `mod` ing the repeating pattern
main :: IO ()
main = do
  lines <- readLines "input/day12.in"
  let (init, tMap) = parseInput lines in
   let (plants, extLeft) = applyManyTrans 20 tMap init in
    let (plants2, extLeft2) = applyManyTrans 5000 tMap init in do
      print $ sumPlants plants extLeft;
      print $ sumPlants plants2 extLeft2

type Trans = M.Map Word8 Word8

sumPlants :: BS.ByteString -> Int -> Int
sumPlants plants l = sumPlants' (map fromIntegral $ BS.unpack plants) l
sumPlants' plants l = sum $ zipWith (*) plants $ map (\x -> x - l - 1) [1..(length plants)]

applyManyTrans :: Int -> Trans -> BS.ByteString -> (BS.ByteString, Int)
applyManyTrans n tMap plants = foldr (\_ (p, l)-> uSnd (l+) $ applyTrans tMap p) (plants, 0) [1..n]
                                                  
-- Returns the new plants and how much left did it extend
applyTrans :: Trans -> BS.ByteString -> (BS.ByteString, Int)
applyTrans tMap plants = (BS.append newFirst newLast, BS.length newFirst)
  where
    newFirst = BS.dropWhile isZero $ BS.pack $ map (findInTrans tMap) $ bWindowfy 5 $ BS.append fiveZerosBS (BS.take 2 plants)
    newLast = BS.pack $ map (findInTrans tMap) $ bWindowfy 5 $ BS.append twoZerosBS $ BS.append plants fiveZerosBS

fiveZerosBS :: BS.ByteString
fiveZerosBS = BS.pack $ map fromIntegral [0,0,0,0,0]

twoZerosBS :: BS.ByteString
twoZerosBS = BS.pack $ map fromIntegral [0,0]

findInTrans :: Trans -> BS.ByteString -> Word8
findInTrans tMap bits = tMap M.! (bits2int $ byteString2list bits)

isZero :: Word8 -> Bool
isZero x = x == 0

isEmpty :: Char -> Bool
isEmpty c = '.' == c

ifPlantNumber :: Char -> Int -> Int
ifPlantNumber '.' _ = 0
ifPlantNumber '#' x = x

plant2int :: Char -> Int
plant2int '.' = 0
plant2int '#' = 1

byteString2list :: BS.ByteString -> [Word8]
byteString2list bs = BS.unpack bs

bits2int :: [Word8] -> Word8
bits2int [b1,b2,b3,b4,b5] = (shift b1 4) + (shift b2 3) + (shift b3 2) + (shift b4 1) + b5

bWindowfy :: Int -> BS.ByteString -> [BS.ByteString]
bWindowfy n l = windowfy' n l []
  where
    windowfy' n l acc =
      let first = BS.take n l in
      if BS.length first == n
      then windowfy' n (BS.tail l) $ first:acc
      else reverse acc


--
-- Parsing the input
--

parseInput :: [String] -> (BS.ByteString, Trans)
parseInput (rawInit:_:rest) = (init, M.fromList $ map parseLine rest)
  where init = BS.pack $ map (fromIntegral . plant2int) $ last . words $ rawInit

parseLine :: String -> (Word8, Word8)
parseLine line = (bits2int (map (fromIntegral . plant2int) from), fromIntegral $ plant2int to)
  where [from, _, [to]] = words line
