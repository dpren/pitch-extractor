module Utils.Misc where

import Numeric (showFFloat)
import Data.List.Split (splitOn)


dropDotFiles :: [FilePath] -> [FilePath]
dropDotFiles = filter (\x -> head x /= '.')

formatDouble :: Int -> Double -> String
formatDouble numOfDecimals floatNum = showFFloat (Just numOfDecimals) floatNum ""

strToDbls :: String -> [Double]
strToDbls = map read . (splitOn ",")
