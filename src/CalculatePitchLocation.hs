module CalculatePitchLocation where

import PitchTrack.Track       (trackFileToList)
import Data.List
import Data.Function          (on)
import Data.Fixed             (mod')
import Numeric.Statistics

import Utils.FreqToNote


a <$$> b = (fmap.fmap) a b

-- | How far apart in pitch to allow when grouping
pitchRangeTolerance :: Double
pitchRangeTolerance = 0.68

-- | 1.059463094359
twelfthRootOfTwo = 2 ** (1/12)

halfStepDownDistFrom :: Double -> Double
halfStepDownDistFrom hz = hz - (hz / twelfthRootOfTwo)


-- | List of pitch samples from '.raw' file, grouped by consecutive similarity
trackFileToBins :: FilePath -> IO [[Double]]
trackFileToBins file = groupByQuantize <$> (trackFileToList file)

trackFileToMidiBins :: FilePath -> IO [[Double]]
trackFileToMidiBins file = groupByEq <$> freqToMidiFloat <$$> (trackFileToList file)

groupByEq :: [Double] -> [[Double]]
groupByEq = groupBy (==)

groupByQuantize :: [Double] -> [[Double]]
groupByQuantize = groupBy ((==) `on` quantize pitchRangeTolerance)

-- | Rounds n down to the nearest multiple of note range
quantize :: Double -> Double -> Double
quantize range n = n - (n `mod'` (max 3 (fromIntegral . round $ range * halfStepDownDistFrom n)))


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

-- | Exclude non-musical pitches before retrieving the longest segment
dropOutliers :: [[Double]] -> [[Double]]
dropOutliers = fmap (takeWhile (\x -> x > 55 && x < 2000))

dropMIDIOutliers :: [[Double]] -> [[Double]]
dropMIDIOutliers = fmap (takeWhile (\x -> x > 11))


-- | totalNumOfSamples / samplesPerSecond = total seconds
computeTime :: [Double] -> Double
computeTime pitchSeg = (totalSamples pitchSeg) / 44100

-- | Each pitch computation is 2048 samples
totalSamples :: [Double] -> Double
totalSamples = fromIntegral . (* 2048) . length



trackRoundFileToBins :: FilePath -> IO [[Double]]
trackRoundFileToBins file = groupByQuantize <$> (trackRound file)

trackRound :: FilePath -> IO [Double]
trackRound file = fmap (fromIntegral . round) <$> trackFileToList file



pitchNoteName :: [Double] -> [Char]
pitchNoteName pitchSeg = freqToNoteName $ median pitchSeg

pitchNoteNameMIDI :: [Double] -> [Char]
pitchNoteNameMIDI pitchSeg = midiToNoteName $ round $ median pitchSeg
