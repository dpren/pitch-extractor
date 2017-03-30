YouTube Pitch Extractor
---

It finds segments of YouTube videos that have a musical pitch and extracts
them to a directory, indexed by note.


Prerequisites
----

    brew install haskell-stack
	brew install youtube-dl
    brew install ffmpeg

    pip install git+https://github.com/aubio/aubio.git
    pip install ffmpeg-normalize


Usage
----

    stack setup
    stack build
    stack exec pitch-extractor-exe "flute exercise" "10"


args:
- search query
- max videos to download (< 50)

(downloading can take a while)

outputs to -> `/vid-ouput/<search query>`
