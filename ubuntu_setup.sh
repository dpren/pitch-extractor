#!/bin/sh
# apt update
# apt full-upgrade -y

# apt install -y python python3 python3-pip ffmpeg

# pip3 install aubio==0.4.9 ffmpeg-normalize==1.15.6

# sudo curl -L https://yt-dl.org/downloads/latest/youtube-dl -o /usr/local/bin/youtube-dl
# sudo chmod a+rx /usr/local/bin/youtube-dl

# curl -sSL https://get.haskellstack.org/ | sh
# exec $SHELL


# If you have > 1.5GB memory:
#=====================================
stack setup
stack build

# else uncomment this instead:
#=====================================

# sudo mkdir -v /var/cache/swap
# cd /var/cache/swap
# sudo dd if=/dev/zero of=swapfile bs=1K count=4M
# sudo chmod 600 swapfile
# sudo mkswap swapfile
# sudo swapon swapfile
# cd ~/pitch-extractor

# stack setup
# stack build

# cd /var/cache/swap
# sudo swapoff swapfile
# cd ~/pitch-extractor



stack exec pitch-extractor-exe "cockatiel" "1000"
stack exec pitch-extractor-exe "simpsons" "1000"
stack exec pitch-extractor-exe "dog playing piano" "1000"
stack exec pitch-extractor-exe "reptiles" "1100"

sudo curl -L https://yt-dl.org/downloads/latest/youtube-dl -o /usr/local/bin/youtube-dl && sudo chmod a+rx /usr/local/bin/youtube-dl