module Util.Misc where

import Numeric               (showFFloat)
import Data.List.Split       (splitOn)
import qualified Turtle as T
import Data.Text as Text     (pack, unpack, replace, head, Text)
import Data.Attoparsec.Text  (parseOnly, signed, decimal)
import TextShow              (showt)
import Numeric               (showFFloat)
import Control.Monad         (when, (<=<))
import Control.Monad.Fail    (MonadFail)
import Data.Monoid           ((<>))
import qualified Shelly as S (shelly, which, toTextWarn)

parseInt :: (Integral a, Monad m, MonadFail m) => Text -> m a
parseInt = handle . parseOnly (signed decimal)

handle :: (Monad m, MonadFail m) => Either String a -> m a
handle e = case e of
  Left errMsg -> fail errMsg
  Right a -> return a

formatDouble :: Int -> Double -> T.Text
formatDouble numOfDecimals floatNum = showt $ showFFloat (Just numOfDecimals) floatNum ""

strToDbls :: String -> [Double]
strToDbls = map read . (splitOn ",")

toTxt :: T.FilePath -> T.Text
toTxt = T.format T.fp

-- note: can't contain newlines
echoTxt :: T.MonadIO io => Text -> io ()
echoTxt = T.echo . T.unsafeTextToLine

isDotFile :: T.FilePath -> Bool
isDotFile x = Text.head (toTxt x) /= '.'

dropDotFiles :: [T.FilePath] -> [T.FilePath]
dropDotFiles = filter isDotFile

exec :: T.MonadIO io => Text -> io (T.ExitCode, Text)
exec cmd = T.shellStrict cmd T.empty


mkdirDestructive :: T.MonadIO io => T.FilePath -> io ()
mkdirDestructive path = do
  dirExists <- T.testdir path
  when dirExists (T.rmtree path)
  T.mkdir path

mkdirUniq :: T.MonadIO io => T.FilePath -> io ()
mkdirUniq = T.mkdir <=< uniqPathName

uniqPathName :: T.MonadIO io => T.FilePath -> io T.FilePath
uniqPathName path = do
  dirExists <- T.testdir path
  case dirExists of
    False -> return path
    True -> uniqPathNumbered path 0
      where
        uniqPathNumbered :: T.MonadIO io => T.FilePath -> Int -> io T.FilePath
        uniqPathNumbered path i = do
          let ip1 = i + 1
              ip1path = unpack ((toTxt path) <> "-" <> (showt ip1))
          enumDirExists <- T.testdir ip1path
          case enumDirExists of
            True -> uniqPathNumbered path ip1
            False -> return ip1path


getPythonPath :: T.MonadIO io => io Text
getPythonPath = S.shelly $ do
    maybP <- S.which "python3"
    case maybP of
        Nothing -> error "Error: python3 not found in path."
        Just p -> S.shelly $ S.toTextWarn p

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)
