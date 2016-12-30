YouTube Pitch Extractor
---

It finds segments of YouTube videos that have a musical pitch and extracts
them to a directory, indexed by note.


Prerequisites
----

    brew install ffmpeg
    brew install youtube-dl
    brew install haskell-stack

    pip install git+https://github.com/aubio/aubio.git


Usage
----

    stack build
    stack exec pitch-extractor-exe "flute exercise" "20"


args:
- search query
- max videos to download (< 50)

(it takes a while)

outputs to -> `/vid-ouput/<search query>`
