module PitchLocation where

import Data.List
import Data.Function (on)

minDuration = 0.5

pitchStartTime :: [Double] -> [[Double]] -> Maybe Double
pitchStartTime segment bins = computeTime <$> (segmentPrefix segment bins)

-- | The list preceding the specified pitch segment
segmentPrefix :: [Double] -> [[Double]] -> Maybe [Double]
segmentPrefix segment bins = concat <$> flip take bins <$> segIndex
    where segIndex = elemIndex segment bins

qualifiedPitchSegments :: [[Double]] -> [[Double]]
qualifiedPitchSegments = filterQualified . dropMIDIOutliers
    where filterQualified = filter isAboveMinDuration

isAboveMinDuration :: [Double] -> Bool
isAboveMinDuration pitchSeg = (computeTime pitchSeg) > minDuration

-- | Filters a reasonable note range
dropMIDIOutliers :: [[Double]] -> [[Double]]
dropMIDIOutliers = fmap (takeWhile (\x -> x >= 24 && x <= 84)) -- G1 - G6

-- | totalNumOfSamples / samplesPerSecond = total seconds
computeTime :: [Double] -> Double
computeTime pitchSeg = (totalSamples pitchSeg) / 44100

-- | Each pitch computation is 2048 samples
totalSamples :: [Double] -> Double
totalSamples = fromIntegral . (* 2048) . length
