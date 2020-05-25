
module Data.Scores where

--
-- Important type definitions for working with representations of scores
--

import Vivid
import Scales.Generic

-- Note: Some of the definitions here have to do with playback,
-- so it would probably be good to have them in a seperate Data.Scores.Playback
-- module

type Vol = Double
type Duration = Rational
data Inst = VividSynth (Vol -> SynthDef '["note"]) | FluidInst String Int
data Line' note = Line' Inst [(Maybe (note, Vol), Duration)]
type Score' note = [Line' note]

-- some useful type synonyms
-- Note: I'll want to refactor types such as Line22 out of the codebase eventually
type Line  note = [(note, Rational)]
type Score note = [Line note]