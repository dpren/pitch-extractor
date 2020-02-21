#!/bin/sh
# apt update
# apt full-upgrade -y

# apt install -y python3 python3-pip ffmpeg youtube-dl

# pip3 install aubio==0.4.9 ffmpeg-normalize==1.15.6

# git clone https://github.com/dpren/pitch-extractor.git
# cd pitch-extractor

# stack setup
# stack build


#==========================================
# Only required if you have < 1.5GB memory:
#==========================================
cd
sudo mkdir -v /var/cache/swap
cd /var/cache/swap
sudo dd if=/dev/zero of=swapfile bs=1K count=4M
sudo chmod 600 swapfile
sudo mkswap swapfile
sudo swapon swapfile
cd
cd pitch-extractor

stack setup
stack build

cd /var/cache/swap
sudo swapoff swapfile
cd
cd pitch-extractor
