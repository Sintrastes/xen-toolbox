
Xen-toolbox
===========

Xen-toolbox is Haskell library meant to facilitate the composition and playback of [xenharmonic](en.xen.wiki) (also known as microtonal) music.

Features/Usage Notes
--------------------

Xentoolbox works by providing a multiparameter `TuningSystem notes group` typeclass, with the `group` type constrained with another typeclass, `AbstractTemperament group`. The idea is that the `notes` in a `TuningSystem` correspond to particular frequencies. The `TuningSystem` typeclass then provide various functions for converting to and from various frequency representations (i.e. fractional midi note numbers, raw frequency in hz), which can then be used by the [fluidsynth](http://www.fluidsynth.org/) (for the playback of soundfonts) and [vivid](http://hackage.haskell.org/package/vivid) (i.e. supercollider) backends for sound generation.

To implement a `Tuningsystem notes group` instance, `group` must implement `AbstractTemperament` and `Group` instances. The idea behind this typeclass being that whereas the `notes` of a `Tuningsystem` are concrete frequencies that can be playedback, the `group` elements of an `AbstractTemperament` represent intervals in that temperament. An `AbstractTemperament g` implements a function `intfreq :: g -> Double` which should be a *group homomorphism*, i.e. for all elements `a,b` of type `g`, it should satisfy `intfreq (a <> b) = intfreq a <> intfreq b`.

Xentools includes several convienient template haskell quasiquoters to facilitate the notation of music in many different temperaments. For example, the following expression gives a value of type `Line NoteBP`, which is a type synonym for `[(NoteBP, Duration)]` -- i.e. a sequence of notes in the Bohlen Pierce tuning, each played for a given `Duration`, which is simply a newtype for a `Rational` from `Data.Ratio`:

```haskell
[lineBP| C3 1, D3 1, E3 1, F3 1, G3 1, H3 1, J3 1, A3 1, B3 1, C4 4 |]
```

Most of the template haskell quasiquoters in xentools use the standard western note names (C, D, E, F, G, A, B), but the above example also uses "H" and "J", which are commonly used to notate the Bohlen Pierce scale. The general format of the quasiquoters in xentools is to parse a comma-seperated list of notes, formatted as: `[natural note name][period number][(optional) list of accidentals] [space] [duration]` For example, for our 22edo parser, we use the standard western natural note names, the notation "Is" and "Es" for sharps and flats, respectively, and the additional microtonal accidentals:

  * "Qis": Quarter sharp
  * "Qes": Quarter flat
  * "Sis": Sesqui (one-and-a-half) sharp
  * "Ses": Sesqui (one-and-a-half) flat
  
Here is an example to see this notation in action:

```haskell
[line22| E4 2, G4 2, E4 2, GEs4 2%3, GEsQes4 2%3, GEsQesQes4 2%3 |]
```

Also, notice that rationals inside the quasiquoter are notated the same way that they would be notated in Haskell.

Currently there are several interfaces provided for playing back `Score`s of notes in different tuning systems. The most versatile of these is the function `sequenceNotes'`, and it's usage is documented in `tests/examples.hs`. However, given that xen-tools is currently in pre-release, this interface is liable to change.

Transformations
---------------

Given some `Line note`s or `Score note`s, we can preform various transformations on them, which can be found in the `Scales.Generic` module provided by xentools. Notably, scores form a [`Semiring`](https://en.wikipedia.org/wiki/Semiring) (from `Data.Semiring` in the `semiring-simple` package). In other words, this means that we can combine two scores of the same type *in sequence* by using the `<.>` operator:

```haskell
-- two melodic fragments in 12 tone equal temperament
a = [[line12| C4 2, D4 2, E4 2, C4 2 |]]
b = [[line12| E4 2, F4 2, G4 4 |]]
-- a line in 12 tone equal temperament where first "a" is played, immediately
-- followed by "b"
abSeq = a <.> b 
```

And we can play two scores of the same type *in parallel* by using `<+>`:

```haskell
c = [[line12| G4 1, A4 1, G4 1, F4 1, E4 2, C4 2 |]]
d = [[line12| C4 2, G3 2, C4 4 |]]
frereJacque = a <.> a <.> b <.> b <.> c <.> c <.> d <.> d
frereJacqueRound = (rest 8 <.> frereJacque) <+> frereJacque 
```


Installation Instructions
-------------------------

Note: Xentools currently requires GHC version 8.4.1 or greater to install. On Ubuntu, the easiest way to make sure this is installed is to run: 

    sudo add-apt-repository -y ppa:hvr/ghc
    sudo apt update
    sudo apt install ghc-8.4.1

Next, ensure that you have cabal, supercollider, and fluidsynth installed on your machine. On recent versions of Ubuntu, to accomplish this you should be able to run:

    sudo apt install supercollider
    sudo apt install ghc
    cabal update
    cabal install vivid

Finally, clone into this repo and run cabal build.

    git clone https://github.com/Sintrastes/xen-toolbox
    cd xen-toolbox
    cabal build

To ensure that everything is working correctly, you can listen to some examples by running:

    ./start_servers.sh &       # to start the supercollider and fluidsynth servers
    cabal run examples

Note: Currently, to get the examples running properly, you need to have a "test.sf3" soundfont file in the same directory as this README.

