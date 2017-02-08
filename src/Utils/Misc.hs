module Utils.Misc where

import Numeric           (showFFloat)
import Data.List.Split   (splitOn)
import qualified Turtle as T
import Data.Text as Text (pack, unpack, replace, head, Text)
import TextShow          (showt)
import Numeric           (showFFloat)


formatDouble :: Int -> Double -> T.Text
formatDouble numOfDecimals floatNum = showt $ showFFloat (Just numOfDecimals) floatNum ""

strToDbls :: String -> [Double]
strToDbls = map read . (splitOn ",")


toTxt :: T.FilePath -> T.Text
toTxt = T.format T.fp

isDotFile :: T.FilePath -> Bool
isDotFile x = Text.head (toTxt x) /= '.'

dropDotFiles :: [T.FilePath] -> [T.FilePath]
dropDotFiles = filter isDotFile

exec :: T.MonadIO io => Text -> io (T.ExitCode, Text)
exec cmd = T.shellStrict cmd T.empty
