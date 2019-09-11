
module Scales.Eq12 where

import Scales.Generic

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
