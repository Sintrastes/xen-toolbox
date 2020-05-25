{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, FlexibleContexts, MultiParamTypeClasses, AllowAmbiguousTypes, FunctionalDependencies, DeriveLift, GeneralizedNewtypeDeriving #-}

module Scales.Generic where

import Data.Monoid
import Data.Monoid.Action
import Data.Group
import Language.Haskell.TH.Lift

-- I'm not sure why this isn't default, but...
instance Group Int where
  invert n = -n
instance Monoid Int where
  mempty = 0
instance Semigroup Int where
  n <> m = n + m

-- TODO: Put in a seperate file and use these definitions throughout the codebase.

newtype Frequency = Frequency Double

instance Group Frequency where
  invert (Frequency f) = Frequency (1/f)
instance Monoid Frequency where
  mempty = Frequency 1
instance Semigroup Frequency where
  (Frequency n) <> (Frequency m) = Frequency $ n * m

newtype Cents = Cents Double

instance Group Cents where
  invert (Cents n) = Cents $ -n
instance Monoid Cents where
  mempty = Cents 0
instance Semigroup Cents where
  (Cents n) <> (Cents m) = Cents $ n + m

newtype MIDISteps = MIDISteps Double

instance Group MIDISteps where
  invert (MIDISteps n) = MIDISteps $ -n
instance Monoid MIDISteps where
  mempty = MIDISteps 0
instance Semigroup MIDISteps where
  (MIDISteps n) <> (MIDISteps m) = MIDISteps $ n + m

steps :: Double -> Double
steps f = 12 * (log f / log 2)

freqToMIDISteps :: Frequency -> MIDISteps
freqToMIDISteps (Frequency f) = MIDISteps $ 12 * (log f / log 2)

midiStepsToFreq :: MIDISteps -> Frequency
midiStepsToFreq (MIDISteps n) = Frequency $ 2**(n/12) 

midiStepsToCents :: MIDISteps -> Cents
midiStepsToCents (MIDISteps n) = Cents $ n*100

centsToMIDISteps :: Cents -> MIDISteps
centsToMIDISteps (Cents c) = MIDISteps $ c/100

freqToCents :: Frequency -> Cents
freqToCents (Frequency f) = Cents $ 1200 * (log f / log 2)

centsToFreq :: Cents -> Frequency
centsToFreq (Cents c) = Frequency (2**(c/1200))

type Octave = Int

-- some useful type synonyms
type RelLine   g    = [(g, Rational)] 
type RelMelody g    = [(g, Rational)]
type RelChord  g    = ([g], Rational)
type RelScore  g    = [RelLine g]
type Melody    note = [(note,  Rational)]
type Chord     note = ([note], Rational) 

class Group g => AbstractTemperament g where
  -- Note, there should be a distinction here
  -- between the additive and the multiplicative
  -- monoids, as well as conversions for different units
  -- to make the code more type-safe
  intfreq :: g -> Double

-- Note: I'm not sure if this is actually possible. Maybe with
-- Template Haskell
-- class Group g => RegularTemperament g where
--   period    :: Double
--   generator :: Double
--   coords    :: g -> (Int,Int)
-- 
-- instance {-# OVERLAPPING #-} RegularTemperament g => AbstractTemperament g where
--   intfreq n = generator*x' + period*y'
--     where (x, y) = coords n
--           x' = fromIntegral x
--           y' = fromIntegral y

class (AbstractTemperament g, Eq n) => TuningSystem n g | n -> g where
  nfreq         :: n -> Double
  fromIntervals :: g -> n
  toIntervals   :: n -> g

instance TuningSystem n g => Action g n where
  act x note = fromIntervals $ x <> toIntervals note 

-- Note: From the above data, we should be able to *derive* an action of
-- g on n, which I think is the better way to structure these classes.

-- Basic note datatype
data Natural = A | B | C | D | E | F | G
  deriving(Show, Eq, Lift)

data BPNatural = Ab | Bb | Cb | Db | Eb | Fb | Gb | Hb | Jb
  deriving(Show, Eq, Lift)

data StdAccidental = Sharp | Flat
  deriving(Show, Eq, Lift)

data QuarterAccidental =  Is  -- Sharp
                  | Es  -- Flat
                  | Qis -- Quarter sharp
                  | Qes -- Quarter flat
                  | Sis -- Sesqui (one-and-a-half) sharp
                  | Ses -- Sesqui (one-and-a-half) flat
  deriving(Show, Eq, Lift)
