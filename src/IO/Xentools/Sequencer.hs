{-# LANGUAGE MultiParamTypeClasses, KindSignatures, TypeOperators, TypeFamilies, FlexibleInstances, DataKinds, ExtendedDefaultRules, UndecidableInstances, GADTs, AllowAmbiguousTypes, TypeInType, ScopedTypeVariables, FunctionalDependencies, QuasiQuotes, TemplateHaskell #-}

module IO.Xentools.Sequencer where

import Scales.Generic
import Scales.Transformations
import Data.Semiring
import Data.Monoid
import Data.Kind
import Data.Ratio
import Data.Semiring
import Vivid
import Control.Concurrent.Async
import Control.Concurrent
import Control.Monad
import System.Environment
import System.Cmd
import Synths.MyVividSynths
import Parsers.Eq22
import Parsers.Eq24
import Parsers.BP
import qualified Data.ByteString.Char8 as C
import Network.Simple.TCP

-- Pitch bend units (in cents)
pbu = 0.0244140625
-- Pitch bend units (in fractional midi note numbers)
pbu' = pbu/100

scale = [60, 62, 64, 65, 67, 69, 71, 72]

noteAtVolume :: TuningSystem note g
  => Vol
  -> (note, Duration)
  -> (Maybe (note, Vol), Duration)
noteAtVolume v (n, d) = ((Just (n, v)), d)

withInst :: TuningSystem note g
  => Vol
  -> Inst
  -> Score note
  -> Score' note
withInst vol inst ls = map (toLine' vol inst) ls
  where toLine' vol inst notes = Line' inst (map (noteAtVolume vol) notes)

atVolume :: TuningSystem note g
  => Vol
  -> [(note, Duration)]
  -> [(Maybe (note, Vol), Duration)]
atVolume vol line = map (\(n,d) ->
   (Just (n, vol), d)) line

sequenceNotes' :: TuningSystem note g
  => Socket
  -> Score' note
  -> Tempo
  -> Preformance
sequenceNotes' socket score tempo = 
   go score 0
       -- This preforms each line at a different channel
    where go ls n = Preformance $ do mapConcurrently (\(l,i) -> run $ preformLine socket l i tempo) 
                                       (zip ls [n..n+(length ls)])
                                     return ()

preformLine :: TuningSystem note g
  => Socket
  -> Line' note
  -> Int -- Channel number
  -> Tempo
  -> Preformance
preformLine socket (Line' (FluidInst sfpath p) line) n tempo =
  sequenceLineFluid socket (sfpath, p) line n tempo
preformLine _ (Line' (VividSynth synth)   line) _ tempo =
  sequenceLineVivid synth line tempo

volToVel :: Double -> Int
-- Converts a volume message to a velocity
-- Note: Volumes should be a real number from 0 to 1
volToVel vol = round $ vol*127
                      

