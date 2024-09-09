!/bin/sh
apt update
apt full-upgrade -y

apt install -y python3 python3-pip ffmpeg


sudo apt update
sudo apt install pipx
pipx ensurepath


apt install cu #for yt-dlp

pipx install aubio
pipx install ffmpeg-normalize
pipx install yt-dlp



curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh



# If you have > 1.5GB memory:
#=====================================
# stack setup
# stack build

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



# stack exec pitch-extractor-exe "dog playing piano" "1000"
# stack exec pitch-extractor-exe "Five Finger Family" "800"
# stack exec pitch-extractor-exe "reptiles" "1100"

