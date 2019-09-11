
module Scales.BP where

import Scales.Generic

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

