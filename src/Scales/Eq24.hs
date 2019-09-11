
module Scales.Eq24 where

import Scales.Generic

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
