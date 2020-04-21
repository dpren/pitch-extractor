YouTube Pitch Extractor
---

It finds segments of YouTube videos that have a musical pitch and extracts
them to a directory, indexed by note - which can then be loaded into the sampler and performed with a MIDI keyboard.


Demo:

[![YouTubaphone](https://img.youtube.com/vi/Ag04gHBzJho/1.jpg)](https://www.youtube.com/watch?v=Ag04gHBzJho)
[![demo](https://img.youtube.com/vi/kZSk0LsozFY/2.jpg)](https://www.youtube.com/watch?v=kZSk0LsozFY)

Dependencies
----

    brew install haskell-stack
    brew install ffmpeg
    brew install youtube-dl
    brew install python3

    pip3 install aubio==0.4.9 
    pip3 install ffmpeg-normalize==1.15.6

Make sure these are all on your $PATH

Windows users, try [scoop](https://github.com/lukesampson/scoop#scoop)

Usage
----

    stack setup
    stack build
    stack exec pitch-extractor-exe "choir audition" "100"


args:
- search query
- max videos to download

(downloading takes a long time)

outputs to -> `/vid-ouput/<search query>`

---

After pitch extractor runs...

* Plug in a MIDI keyboard. Or, use a virtual one like MidiKeys for Mac.

* Open `sampler/index.html` in Chrome or Opera.

* Choose an outputted video folder, and rock out.
