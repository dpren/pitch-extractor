module Utils.FreqToNote where

notes :: [[Char]]
notes = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

freqToMidi :: Double -> Int
freqToMidi hz = round $ 69 + 12 * (logBase 2 (hz / 440))

freqToMidiFloat :: Double -> Double
freqToMidiFloat hz = fromIntegral . round $ 69 + 12 * (logBase 2 ((max 8 hz) / 440))

midiToNoteIndex :: Int -> Int
midiToNoteIndex midi = midi `mod` 12

midiToOctave :: Int -> Int
midiToOctave midi = (div midi 12) - 1

midiToNoteName :: Int -> String
midiToNoteName midi = (show $ midiToOctave midi) ++ (notes !! (midiToNoteIndex midi))

-- _midiToNoteName :: Int -> Text
-- _midiToNoteName midi = (show $ midiToOctave midi) ++ (notes !! (midiToNoteIndex midi))


freqToNoteName :: Double -> String
freqToNoteName = midiToNoteName . freqToMidi
