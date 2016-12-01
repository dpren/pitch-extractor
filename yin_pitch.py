#! /usr/bin/env python

import sys
from aubio import source, pitch

if len(sys.argv) < 2:
    print("Usage: %s <filename> [samplerate]" % sys.argv[0])
    sys.exit(1)

filename = sys.argv[1]

samplerate = 44100
if len( sys.argv ) > 2: samplerate = int(sys.argv[2])

win_s = 2048 # fft size
hop_s = 2048 # hop size #512 #win_s/4

s = source(filename, samplerate, hop_s)
samplerate = s.samplerate

tolerance = 0.8

pitch_o = pitch("yin", win_s, hop_s, samplerate)
pitch_o.set_unit("midi")
pitch_o.set_tolerance(tolerance)

pitches = []
confidences = []

# total number of frames read
total_frames = 0
while True:
    samples, read = s()
    pitch = pitch_o(samples)[0]
    pitch = int(round(pitch))
    confidence = pitch_o.get_confidence()
    if confidence < 0.9: pitch = 0
    # print("%f %f %f" % (total_frames / float(samplerate), pitch, confidence))
    pitches += [pitch]
    confidences += [confidence]
    total_frames += read
    if read < hop_s: break

if 0: sys.exit(0)

sys.stdout.write(str(pitches).replace(' ', '').strip('[]'))
