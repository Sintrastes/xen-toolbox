{-# LANGUAGE MultiParamTypeClasses, KindSignatures, TypeOperators, TypeFamilies, FlexibleInstances, DataKinds, ExtendedDefaultRules, UndecidableInstances, GADTs, AllowAmbiguousTypes, TypeInType, ScopedTypeVariables, FunctionalDependencies, QuasiQuotes, TemplateHaskell #-}

module Data.Xentools.ExampleScores where

import Scales.Generic
import Scales.Transformations
import Data.Semiring
import Synths.MyVividSynths
import Parsers.Eq22
import Parsers.Eq24
import Parsers.BP

exBPScore = [Line' (VividSynth tone)
                   (atVolume 1 ([lineBP| C3 1, D3 1, E3 1, F3 1, G3 1, H3 1, J3 1, A3 1, B3 1, C4 4 |] :: Line NoteBP)),
             Line' (VividSynth tone)
                   (atVolume 1 ([lineBP| E3 4, F3 4, C3 5 |])),
             Line' (VividSynth tone)
                   (atVolume 1 ([lineBP| G3 4, J3 4, E3 5 |]))]

-- The same example, but written with the withInst function
-- exscore1' :: Score Note22 
{- 
exscore1' = [  withInst 0.3 (VividSynth additiveSynth) ([line22| C4 2, D4 2, EQes4 1, BQes3 1, C4 4, C4 2, D4 2, EQes4 1, BQes3 1, C4 8 |] :: Line Note22)
             , withInst 0.5 (VividSynth tone)          ([line22| C5 3, D5 1, EQes5 4, F5 2, DQes5 2, EQes5 4, EQes4 8 |] :: Line Note22)
             , withInst 0.4 (FluidInst "test.sf3" 17)  ([line22| C4 2, C4 2, C4 2, C4 2, C4 16 |] :: Line Note22)
             , withInst 0.4 (FluidInst "test.sf3" 17)  ([line22| EQes3 2, EQes3 2, EQes3 2, EQes3 2, EQes3 16 |] :: Line Note22)
             , withInst 0.4 (FluidInst "test.sf3" 17)  ([line22| G4 2, G4 2, G4 2, G4 2, G4 16 |] :: Line Note22)
             ] 
-}

ex22EdoScore1 = [Line' (VividSynth additiveSynth)
                 (atVolume 0.3 ([line22| C4 2, D4 2, EQes4 1, BQes3 1, C4 4, C4 2, D4 2, EQes4 1, BQes3 1, C4 8 |] :: Line Note22) ),
           Line' (VividSynth tone)
                 (atVolume 0.5 ([line22| C5 3, D5 1, EQes5 4, F5 2, DQes5 2, EQes5 4, EQes4 8 |])),
           Line' (FluidInst "test.sf3" 17)
                 (atVolume 0.4 ([line22| C4 2, C4 2, C4 2, C4 2, C4 16 |])),
           Line' (FluidInst "test.sf3" 17)
                 (atVolume 0.4 ([line22| EQes3 2, EQes3 2, EQes3 2, EQes3 2, EQes3 16 |])),
           Line' (FluidInst "test.sf3" 17)
                 (atVolume 0.4 ([line22| G4 2, G4 2, G4 2, G4 2, G4 16 |]))
           --Line' (FluidInst "test.sf3" 9)
           --      (atVolume 1 ([line22| C6 4, EQes6 4, F6 4, DQes6 4, C6 8  |]))
          ]

ex22EdoScore2 = [Line' (VividSynth additiveSynth)
                 (atVolume 0.3 ([line22| C4 2, D4 2, EQes4 1, BQes3 1, C4 4, C4 2, D4 2, EQes4 1, BQes3 1, C4 8 |] :: Line Note22) ),
           Line' (VividSynth tone)
                 (atVolume 0.5 ([line22| C5 3, D5 1, EQes5 4, F5 2, DQes5 2, EQes5 4, EQes4 8 |])),
           Line' (FluidInst "test.sf3" 17)
                 (atVolume 0.4 ([line22| C4 2, C4 2, C4 2, C4 2, C4 16 |])),
           Line' (FluidInst "test.sf3" 17)
                 (atVolume 0.4 ([line22| EQes3 2, EQes3 2, EQes3 2, EQes3 2, EQes3 16 |])),
           Line' (FluidInst "test.sf3" 17)
                 (atVolume 0.4 ([line22| G4 2, G4 2, G4 2, G4 2, G4 16 |]))
           --Line' (FluidInst "test.sf3" 9)
           --      (atVolume 1 ([line22| C6 4, EQes6 4, F6 4, DQes6 4, C6 8  |]))
          ]