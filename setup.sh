#!/bin/sh

if ! type "brew" > /dev/null; then
    echo "Homebrew is required, to install it, run this command and try again: "
    echo '  /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"'
    exit 1;
fi

brew install haskell-stack &&
brew install youtube-dl &&
brew install ffmpeg &&
brew install python &&
pip install aubio &&
pip install ffmpeg-normalize &&

echo "setup done"
