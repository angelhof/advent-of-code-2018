module Day4 where

import Lib
import Data.List
import Data.List.Split
import qualified Data.Map as Map

main :: IO ()
main = do
  lines <- readLines "input/day4.in"
  let events = sort $ map parseLine lines in
   let gst = foldGuardSleepTimes events in
    print (result1 gst, result2 gst)


-- For each guard I keep the total time that they slept
-- and a map from minutes to the frequency of them being
-- asleep that minute

type GuardsSleepTimes = Map.Map Int (Int, Map.Map Int Int)

-- Returns the first result, that is the product of the
-- id of the guard who has slept the most and the
-- minute that they have slept the most at
result1 :: GuardsSleepTimes -> Int
result1 gst = guardId1 * guardMostSleptMinute1
  where
    (guardId1, guardSleep1) = mostSleepingGuard gst
    (guardMostSleptMinute1, guardMostSleptMinuteFreq1) = mostSleptMinute guardSleep1

-- Returns the second result, that is the product of the
-- id of the guard who has slept the most during any specific minute
-- and the minute that they have slept the most at
result2 :: GuardsSleepTimes -> Int
result2 gst = guardId2 * guardMostSleptMinute2
  where
    (guardId2, (guardMostSleptMinute2, guardMostSleptMinuteFreq2)) = mostFrequentlySleepingGuard gst


-- Returns the Guard that is has slept for the most time
mostSleepingGuard :: GuardsSleepTimes -> (Int, (Int, Map.Map Int Int))
mostSleepingGuard gst = maximumBy (ordGet (fst . snd)) $ Map.toList gst

-- Returns the minute with the largest sleep frequency
mostSleptMinute :: (Int, Map.Map Int Int) -> (Int, Int)
mostSleptMinute (_, sleptMinutes) = maximumBy (ordGet snd) $ Map.toList sleptMinutes

-- Returns the guard with the highest sleeping frequency for any minute
mostFrequentlySleepingGuard :: GuardsSleepTimes -> (Int, (Int, Int))
mostFrequentlySleepingGuard gst = maximumBy (ordGet (snd . snd)) $ Map.toList freqGst
  where
    freqGst = Map.map mostSleptMinute gst

-- Folds over the events and creates thr Guard Sleep Times data structure
-- Assuming that the initial current guard is undefined
foldGuardSleepTimes :: [Event] -> GuardsSleepTimes
foldGuardSleepTimes evs = third $ foldl foldGuardSleepTimes' (undefined, undefined, Map.empty) evs

foldGuardSleepTimes' :: (Int, Ts, GuardsSleepTimes) -> Event -> (Int, Ts, GuardsSleepTimes)
foldGuardSleepTimes' (_, _, gst) (Event ts (Begin id)) = (id, ts, gst)
foldGuardSleepTimes' (id, ts, gst) (Event ts' Sleep) = (id, ts', gst)
foldGuardSleepTimes' (id, ts, gst) (Event ts' Wake) = (id, ts', wakeUp id ts ts' gst)

wakeUp :: Int -> Ts -> Ts -> GuardsSleepTimes -> GuardsSleepTimes
wakeUp id ts ts' = Map.alter upd' id
  where
    diff = minutesDiff ts ts'
    mMap = minutesMap ts ts'
    upd' Nothing = Just (diff, mMap)
    upd' (Just (oDiff, omMap)) = Just (diff + oDiff, Map.unionWith (+) omMap mMap)

-- Assuming that only differences with different minutes will be asked
minutesDiff :: Ts -> Ts -> Int
minutesDiff (Ts _ _ _ _ m1) (Ts _ _ _ _ m2) = m2 - m1

minutesMap :: Ts -> Ts -> Map.Map Int Int
minutesMap (Ts _ _ _ _ m1) (Ts _ _ _ _ m2) = foldl' upd Map.empty [m1..(m2-1)]
  where
    upd map min = Map.alter upd' min map
    upd' Nothing = Just 1
    upd' (Just om) = Just (om + 1)

--
-- Parsing the input
--
  
data Ts = Ts Int Int Int Int Int
          deriving (Eq, Ord, Show)

data Action = Begin Int
            | Wake
            | Sleep
              deriving (Eq, Show)
                       
data Event = Event Ts Action
             deriving (Show)

instance Eq Event where
  (Event t1 a1) == (Event t2 a2) = (t1 == t2) && (a1 == a2)

instance Ord Event where
    compare (Event t1 _) (Event t2 _) = compare t1 t2 

parseLine :: String -> Event
parseLine line = Event (parseTs ts) (parseAction action)
  where
    ['[':ts,' ':action] = splitOn "]" line

parseTs :: String -> Ts
parseTs ts = Ts (read year) (read month) (read day) (read hour) (read minute)
  where
    (year, '-':yearRest) = splitAt 4 ts
    (month, '-':monthRest) = splitAt 2 yearRest
    (day, ' ':dayRest) = splitAt 2 monthRest
    (hour, ':':minute) = splitAt 2 dayRest

parseAction :: String -> Action
parseAction "wakes up" = Wake
parseAction "falls asleep" = Sleep
parseAction begin = Begin (read id)
  where
    [_, '#':id, _, _] = splitOn " " begin
