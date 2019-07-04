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
type Line note      = [(note, Rational)]
type RelLine   g    = [(g, Rational)] 
type RelMelody g    = [(g, Rational)]
type RelChord  g    = ([g], Rational)
type RelScore  g    = [RelLine g]
type Melody    note = [(note,  Rational)]
type Chord     note = ([note], Rational) 
type Score     note = [Line note]

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

----- Bohlen Pierce Instances -------

data PitchClassBP = PitchClassBP BPNatural [StdAccidental]
  deriving(Show, Eq, Lift)

type NoteBP = (PitchClassBP, Int)

newtype EdtBP = EdtBP (Int, Int)
  deriving(Semigroup, Monoid, Group)

stepSizeBP = 146.30/100

fromIntervalsBP :: EdtBP -> NoteBP
fromIntervalsBP (EdtBP (a,b)) = (pc a, b)
  where pc a = [PitchClassBP Cb [],
                PitchClassBP Db [Flat],
                PitchClassBP Db [],
                PitchClassBP Eb [],
                PitchClassBP Fb [],
                PitchClassBP Gb [Flat],
                PitchClassBP Gb [],
                PitchClassBP Hb [],
                PitchClassBP Jb [Flat],
                PitchClassBP Jb [],
                PitchClassBP Ab [],
                PitchClassBP Bb [Flat],
                PitchClassBP Bb []] !! a

toIntervalsBP   :: NoteBP -> EdtBP
toIntervalsBP n = let (x,y) = divMod (bpToSteps n) 13 in EdtBP (y,x)

bpToSteps    :: NoteBP -> Int
bpToSteps (pc, o) = o*13 -- Add the correct number of octaves to the midi note number
                          + pc_to_n_steps pc -- Play the correct note in the octave
  where { pc_to_n_steps (PitchClassBP nat acs) =
           (val' nat) + sum (map val acs);
          val Sharp  =  1; val Flat =  -1;
          val' Cb = 0; val' Db = 2; val' Eb = 3; val' Fb = 4;
          val' Gb = 6; val' Hb = 7; val' Jb = 9; val' Ab = 10; val' Bb = 12 }

instance AbstractTemperament EdtBP where
  intfreq (EdtBP (a, b)) = (fromIntegral a)*stepSizeBP + 2*(fromIntegral b)
instance TuningSystem NoteBP EdtBP where
  nfreq note    = (fromIntegral $ bpToSteps note) * stepSizeBP
  toIntervals   = toIntervalsBP
  fromIntervals = fromIntervalsBP

----- 12edo instances -------

data PitchClass12 = PitchClass12 Natural [StdAccidental]
  deriving(Show, Eq, Lift)

type Note12 = (PitchClass12, Octave)

newtype Edo12 = Edo12 (Int,Int)
  deriving(Semigroup, Monoid, Group)
  
stepSize12edo = 1

fromIntervals12 :: Edo12 -> Note12
fromIntervals12 = undefined
toIntervals12   :: Note12 -> Edo12
toIntervals12 = undefined

edo12ToSteps    :: Note12 -> Int
edo12ToSteps  = undefined

instance AbstractTemperament Edo12 where
  intfreq (Edo12 (a, b)) = (fromIntegral a)*stepSize12edo + 2*(fromIntegral b)
instance TuningSystem Note12 Edo12 where
  nfreq note    = (fromIntegral $ edo12ToSteps note) * stepSize12edo
  toIntervals   = toIntervals12
  fromIntervals = fromIntervals12

----- 24edo instances -------

data PitchClass24 = PitchClass24 Natural [QuarterAccidental]
  deriving(Show, Eq, Lift)

type Note24 = (PitchClass24, Octave)

newtype Edo24 = Edo24 (Int, Int)
  deriving(Semigroup, Monoid, Group)

stepSize24edo = 1

fromIntervals24 :: Edo24 -> Note24
fromIntervals24 (Edo24 (a,b)) = (pc a, b)
  where pc a = [PitchClass24 C [],
                PitchClass24 C [Qis],
                PitchClass24 C [Is],
                PitchClass24 C [Sis],
                PitchClass24 D [],
                PitchClass24 D [Qis],
                PitchClass24 D [Is],
                PitchClass24 D [Sis],
                PitchClass24 E [],
                PitchClass24 E [Qis],
                PitchClass24 F [],
                PitchClass24 F [Qis],
                PitchClass24 F [Is],
                PitchClass24 F [Sis],
                PitchClass24 G [],
                PitchClass24 G [Qis],
                PitchClass24 G [Is],
                PitchClass24 G [Sis],
                PitchClass24 A [],
                PitchClass24 A [Qis],
                PitchClass24 A [Is],
                PitchClass24 A [Sis],
                PitchClass24 B [],
                PitchClass24 B [Qis]] !! a

toIntervals24   :: Note24 -> Edo24
toIntervals24 n = let (x,y) = divMod (edo24ToSteps n) 24 in Edo24 (y,x)

edo24ToSteps    :: Note24 -> Int
edo24ToSteps (pc, o) = o*24 -- Add the correct number of octaves to the midi note number
                          + pc_to_n_steps pc -- Play the correct note in the octave
  where { pc_to_n_steps (PitchClass24 nat acs) =
           (val' nat) + sum (map val acs);
          val Qis  =  1; val Is =  2; val Sis  =  3;
          val Qes  = -1; val Es = -2; val Ses  = -3;
          val' C = 0; val' D = 4; val' E = 8; val' F = 10;
          val' G = 14; val' A = 18; val' B = 22 }


instance AbstractTemperament Edo24 where
  intfreq (Edo24 (a, b)) = (fromIntegral a)*stepSize24edo + 2*(fromIntegral b)
instance TuningSystem Note24 Edo24 where
  nfreq note    = (fromIntegral $ edo24ToSteps note) * stepSize24edo
  toIntervals   = toIntervals24
  fromIntervals = fromIntervals24

----- 19edo instances -------

data PitchClass19 = PitchClass19 Natural [StdAccidental]
  deriving(Show, Eq, Lift)

type Note19 = (PitchClass19, Octave)

newtype Edo19 = Edo19 (Int, Int)
  deriving(Semigroup, Monoid, Group)

stepSize19edo = 63.1579/100

fromIntervals19 :: Edo19 -> Note19
fromIntervals19 (Edo19 (a,b)) = (pc a, b)
  where pc n = [PitchClass19 C [],
                PitchClass19 C [Sharp],
                PitchClass19 D [Flat],
                PitchClass19 D [],
                PitchClass19 D [Sharp],
                PitchClass19 E [Flat],
                PitchClass19 E [],
                PitchClass19 E [Sharp],
                PitchClass19 F [],
                PitchClass19 F [Sharp],
                PitchClass19 G [Flat],
                PitchClass19 G [],
                PitchClass19 G [Sharp],
                PitchClass19 A [Flat],
                PitchClass19 A [],
                PitchClass19 A [Sharp],
                PitchClass19 B [Flat],
                PitchClass19 B [],
                PitchClass19 B [Sharp]] !! n

toIntervals19   :: Note19 -> Edo19
toIntervals19 n = let (x,y) = divMod (edo19ToSteps n) 19 in Edo19 (y,x)

edo19ToSteps    :: Note19 -> Int
edo19ToSteps (pc, o) = o*19 -- Add the correct number of octaves to the midi note number
                          + pc_to_n_steps pc -- Play the correct note in the octave
  where { pc_to_n_steps (PitchClass19 nat acs) =
           (val' nat) + sum (map val acs);
          val Sharp  =  1; val Flat  = -1;
          val' C = 0; val' D = 3; val' E = 6; val' F = 8;
          val' G = 11; val' A = 14; val' B = 17 }

instance AbstractTemperament Edo19 where
  intfreq (Edo19 (a, b)) = (fromIntegral a)*stepSize19edo + 2*(fromIntegral b)
instance TuningSystem Note19 Edo19 where
  nfreq note    = (fromIntegral $ edo19ToSteps note) * stepSize19edo
  toIntervals   = toIntervals19
  fromIntervals = fromIntervals19

----- 22edo instances -------

-- NOTE: I think this should all be in Data22.hs now

data PitchClass22 = PitchClass22 Natural [QuarterAccidental]
  deriving(Show, Eq, Lift)

type Note22 = (PitchClass22, Octave)

newtype Edo22 = Edo22 (Int, Int)
  deriving(Semigroup, Monoid, Group)

stepSize22edo = 0.5455
        
edo22NoteToSteps :: Note22 -> Int
edo22NoteToSteps (pc, o) = o*22 -- Add the correct number of octaves to the midi note number
                          + pc_to_n_steps pc -- Play the correct note in the octave
  where { pc_to_n_steps (PitchClass22 nat acs) =
           (val' nat) + sum (map val acs);
          val Is  =  2; val Es  = -2; val Qis =  1;
          val Qes = -1; val Sis =  3; val Ses = -3;
          val' C = 0; val' D = 4; val' E = 8; val' F = 9;
          val' G = 13; val' A = 17; val' B = 21 }

toIntervals22 :: Note22 -> Edo22
toIntervals22 n = let (x,y) = divMod (edo22NoteToSteps n) 22 in Edo22 (y,x)

fromIntervals22 :: Edo22 -> Note22
fromIntervals22 (Edo22 (a,b)) = (pc a, b)
   where pc n = [PitchClass22 C [], PitchClass22 C [Qis], PitchClass22 C [Is], PitchClass22 C [Sis],
                 PitchClass22 D [], PitchClass22 D [Qis], PitchClass22 D [Is],  
                 PitchClass22 D [Sis], PitchClass22 E [], PitchClass22 F [],    
                 PitchClass22 F [Qis], PitchClass22 F [Is], PitchClass22 F [Sis], 
                 PitchClass22 G [], PitchClass22 G [Qis], PitchClass22 G [Is],  
                 PitchClass22 G [Sis], PitchClass22 A [], PitchClass22 A [Qis], 
                 PitchClass22 A [Is], PitchClass22 A [Sis], PitchClass22 B []] !! n 

instance AbstractTemperament Edo22 where
  intfreq (Edo22 (a, b)) = (fromIntegral a)*stepSize22edo + 2*(fromIntegral b)
instance TuningSystem Note22 Edo22 where
  nfreq note    = ((fromIntegral $ edo22NoteToSteps note) + 22) * stepSize22edo
  toIntervals   = toIntervals22
  fromIntervals = fromIntervals22

-- test1 = act (Edo22 (1,0)) (PitchClass22 C [], 4 :: Int)
-- test2 = (Edo22 (1,2)) <> (Edo22 (21,1))
