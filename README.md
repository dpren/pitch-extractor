YouTube Pitch Extractor
---

It finds segments of YouTube videos that have a musical pitch and extracts
them to a directory, indexed by note - which can then be loaded into the sampler and performed with a MIDI keyboard.


<div style="width: 430px;  height: 222px; overflow: hidden;"><a href="http://www.youtube.com/watch?feature=player_embedded&v=kZSk0LsozFY
" target="_blank"><img src="https://i.ytimg.com/vi_webp/kZSk0LsozFY/sddefault.webp"
alt="demo on youtube" style="position: relative; top: 50%; transform: translateY(-50%);"/></a></div>


Prerequisites
----
OS X quick setup:

    cd pitch-extractor
    sh setup.sh

Which installs these dependencies:

    brew install haskell-stack
    brew install youtube-dl
    brew install ffmpeg
    brew install python
    pip install aubio
    pip install ffmpeg-normalize

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
