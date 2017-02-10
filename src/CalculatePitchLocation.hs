module CalculatePitchLocation where

import Data.List
import Data.Function (on)
-- import Data.Fixed    (mod')


groupByEq :: [Double] -> [[Double]]
groupByEq = groupBy (==)

pitchStartTime :: [[Double]] -> Maybe Double
pitchStartTime bins = computeTime <$> (firstSegment bins)

-- | The list preceding the pitch segment
firstSegment :: [[Double]] -> Maybe [Double]
firstSegment bins = concat <$> flip take bins <$> (longestBinIndex . dropMIDIOutliers) bins

longestPitchSeg :: [[Double]] -> [Double]
longestPitchSeg = longestBin . dropMIDIOutliers


longestBinIndex :: [[Double]] -> Maybe Int
longestBinIndex bins = elemIndex (longestBin bins) bins

longestBin :: [[Double]] -> [Double]
longestBin = (maximumBy (compare `on` length))

-- | Filters a reasonable note range
dropMIDIOutliers :: [[Double]] -> [[Double]]
dropMIDIOutliers = fmap (takeWhile (\x -> x > 19 && x < 79)) -- G1 - G6

-- | totalNumOfSamples / samplesPerSecond = total seconds
computeTime :: [Double] -> Double
computeTime pitchSeg = (totalSamples pitchSeg) / 44100

-- | Each pitch computation is 2048 samples
totalSamples :: [Double] -> Double
totalSamples = fromIntegral . (* 2048) . length
