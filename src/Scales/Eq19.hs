
module Scales.Eq19 where

import Scales.Generic

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
