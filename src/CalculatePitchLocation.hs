module CalculatePitchLocation where

import PitchTrack.Track       (trackFileToList)
import Data.List
import Data.Function          (on)
import Control.Applicative    (liftA2)
import Numeric.Statistics     (median)
import FreqToNote             (freqToNoteName)


pitchNoteName :: [Int] -> [Char]
pitchNoteName pitchSeg = freqToNoteName $ median (fromIntegral <$> pitchSeg)

pitchStartTime :: [[Int]] -> Maybe Double
pitchStartTime bins = computeTime <$> (firstSegment bins)

pitchDuration :: [[Int]] -> Double
pitchDuration bins = computeTime (pitchSegment bins)


firstSegment :: [[Int]] -> Maybe [Int]
firstSegment bins = concat <$> flip take bins <$> (longestBinIndex bins)

pitchSegment :: [[Int]] -> [Int]
pitchSegment bins = longestBin bins


longestBinIndex :: [[Int]] -> Maybe Int
longestBinIndex = liftA2 elemIndex longestBin dropZeros

longestBin :: [[Int]] -> [Int]
longestBin = longest . dropZeros

longest :: [[a]] -> [a]
longest = maximumBy (compare `on` length)

dropZeros :: [[Int]] -> [[Int]]
dropZeros = fmap (takeWhile (> 0))


-- | Data binning by frequency range
trackFileToBins :: FilePath -> IO [[Int]]
trackFileToBins file = groupByQuantize 3 <$> (trackRound file)

groupByQuantize :: Int -> [Int] -> [[Int]]
groupByQuantize range = groupBy ((==) `on` quantize range)

-- | Rounds n down to the nearest multiple of range
quantize :: Int -> Int -> Int
quantize range n = n - (n `mod` range)


-- | totalNumOfSamples / samplesPerSecond = total seconds
computeTime :: [Int] -> Double
computeTime pitchList = (totalSamples pitchList) / 44100

-- | 2048 samples per pitch computation (defaultSampleNum)
totalSamples :: [Int] -> Double
totalSamples = fromIntegral . (* 2048) . length


trackRound :: FilePath -> IO [Int]
trackRound file = fmap round <$> trackFileToList file
