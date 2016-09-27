YouTube Pitch Extractor
---

#### :information_desk_person::game_die::tv::notes:

It finds segments of YouTube videos that have a musical pitch.

A Haskell learning project of mine - also part of a larger program for generative composition.


Prerequisites
----

	brew install ffmpeg
	brew install youtube-dl

[Stack](https://docs.haskellstack.org/en/stable/README/) (Haskell build tool)

Usage
----

    stack build
    stack exec pitch-extractor-exe "singing in the shower" "5"


args:
- search query
- max videos to download (< 50)

(it takes a while)

outputs to -> `/vid-ouput/<search query>`
