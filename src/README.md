
Xen-toolbox
===========

Xen-toolbox is Haskell library meant to facilitate the composition and playback of [xenharmonic](en.xen.wiki) (also known as microtonal) music.

Features/Usage Notes
--------------------

TODO

Installation Instructions
-------------------------

First, ensure that you have cabal, supercollider, and fluidsynth installed on your machine. On recent versions of Ubuntu, to accomplish this you should be able to run:

    sudo apt install supercollider
    sudo apt install ghc
    cabal install vivid

Finally, clone into this repo and run cabal build.

    git clone https://github.com/Sintrastes/xen-toolbox
    cd xen-toolbox
    cabal build

To ensure that everything is working correctly, you can listen to some examples by running:

    ./start_servers.sh &       # to start the supercollider and fluidsynth servers
    cabal run examples

Note: Currently, to get the examples running properly, you need to have a "test.sf3" soundfont file in the same directory as this README.

