module FreqToNote (freqToNoteName) where

notes :: [[Char]]
notes = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]

freqToMidi :: Double -> Int
freqToMidi freq = round $ 69 + 12 * (logBase 2 (freq / 440))

midiToNoteIndex :: Int -> Int
midiToNoteIndex midi = midi `mod` 12

midiToOctave :: Int -> Int
midiToOctave midi = (div midi 12) - 1

midiToNoteName :: Int -> [Char]
midiToNoteName midi = notes !! (midiToNoteIndex midi) ++ (show $ midiToOctave midi)

freqToNoteName :: Double -> [Char]
freqToNoteName = midiToNoteName . freqToMidi
