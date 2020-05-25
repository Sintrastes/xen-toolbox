module Scales.Eq22 where

-- Note: This is some old code that I've written. I'll want to eventually write something
-- more generic than this.

import Scales.Generic (Natural(..), QuarterAccidental(..))

steps :: Double -> Double
-- Converts a frequency ratio into a fractional step size
-- for use by supercollider
steps f = 12 * (log f / log 2)

-- Basic note datatype
-- data Natural = A | B | C | D | E | F | G
--  deriving(Show)

-- Accidental names, using the German conventions,
-- together with "q" prefixes for quarter sharps
-- and flats
--data Accidental =  Is  -- Sharp
--                  | Es  -- Flat
--                  | Qis -- Quarter sharp
--                  | Qes -- Quarter flat
--                  | Sis -- Sesqui (one-and-a-half) sharp
--                  | Ses -- Sesqui (one-and-a-half) flat
--  deriving(Show)
                  
-- For full generality, we will allow here for modifications
-- by multiple accidentals.
data PitchClass22 = PitchClass22 Natural [QuarterAccidental]
  deriving(Show)

type Octave = Int
type Note22 = (PitchClass22, Octave)

data Interval22 = U | Maj2 | Min2 | SMaj2 

type Instrument = String -- Instrument IDs

-- A simple datatype for scores
-- Note: It actually would be nice here for Instrument to be in the
-- type-level. This would make it easier to implement the semigroup instance
-- of the score.

type Line22 = [(Note22, Rational)]
type NoteSequence22 = [Line22]

data Score22 = Score22 [Instrument] [NoteSequence22]
-- Smart constructor for Score 22:
makeScore22 :: [Instrument] -> Score22
-- Makes an empty 22edo score with the given instrument IDs
makeScore22 ins = Score22 ins (replicate (length ins) []) 

-- We should have some sort of "show" method available for these, e.x. so that:
--
-- e.x. (PitchClass C [], 4)       -> "C4"
--      (PitchClass C [Is], 4)     -> "Cis4"
--      (PitchClass C [Qis], 4)    -> "Cquis4"
--      (PitchClass C [Is,Qis], 4) -> "Cisqis4"

stepSize22edo :: Double
stepSize22edo = 0.5455

edo22ToSteps :: Natural -> Double
edo22ToSteps C = 0  * stepSize22edo
edo22ToSteps D = 4  * stepSize22edo
edo22ToSteps E = 8  * stepSize22edo
edo22ToSteps F = 9  * stepSize22edo
edo22ToSteps G = 13 * stepSize22edo
edo22ToSteps A = 17 * stepSize22edo
edo22ToSteps B = 21 * stepSize22edo

edo22PCToSteps :: PitchClass22 -> Double
edo22PCToSteps (PitchClass22 nat acs) =
    (edo22ToSteps nat) + sum (map val acs)
  where val Is  =  2 * stepSize22edo
        val Es  = -2 * stepSize22edo
        val Qis =  1 * stepSize22edo
        val Qes = -1 * stepSize22edo
        val Sis =  3 * stepSize22edo
        val Ses = -3 * stepSize22edo
        
porcupine22ToSteps :: Natural -> Double
porcupine22ToSteps x =
  case x of
     C -> 0  * stepSize22edo
     D -> 3  * stepSize22edo
     E -> 6  * stepSize22edo
     F -> 9  * stepSize22edo
     G -> 13 * stepSize22edo
     A -> 16 * stepSize22edo
     B -> 19 * stepSize22edo

porcupineCScale :: [PitchClass22]
porcupineCScale = []
