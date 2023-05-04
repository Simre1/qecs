rm profile/qecs-profile.*
cabal run --enable-profiling qecs-profile -- +RTS -s -hy &&
mv qecs-profile.* profile &&
cd profile &&
hp2ps -c qecs-profile.hp &&
convert qecs-profile.ps qecs-profile.png

