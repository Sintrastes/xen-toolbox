{-# LANGUAGE MultiParamTypeClasses, KindSignatures, TypeOperators, TypeFamilies, FlexibleInstances, DataKinds, ExtendedDefaultRules, UndecidableInstances, GADTs, AllowAmbiguousTypes, TypeInType, ScopedTypeVariables, FunctionalDependencies, QuasiQuotes, TemplateHaskell #-}

module IO.VividPlaybackUtils where

-- sequenceNotesFluid
-- sequenceNotes
-- sequenceNotes'

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

setProgram :: Socket -> Int -> Int -> IO ()
setProgram socket channel prog = send socket (C.pack $ "prog "++show channel++" "++show prog++"\n")

openServer :: Int -> Int -> IO ()
openServer channels port = do
   system $ "fluidsynth -K "++ show channels ++" -j  -i -s -o \"shell.port="++show port++"\""
   return ()
   
loadSoundFont :: Socket -> String -> IO ()
loadSoundFont socket filepath = send socket (C.pack $ "load "++show filepath++"\n")

noteOn :: Socket -> Int -> Int -> Int -> IO ()
noteOn socket channel note vel = send socket (C.pack $ "noteon "++show channel++" "++show note++" "++show vel++"\n")

noteOff :: Socket -> Int -> Int -> IO ()
noteOff socket channel note = send socket (C.pack $ "noteoff "++show channel++" "++show note++"\n")

pitchBendSet :: Socket -> Int -> Int -> IO ()
pitchBendSet socket channel value = send socket (C.pack $ "pitch_bend "++show channel++" "++show value++"\n")


  
microNoteOn :: Socket -> Int -> Double -> Int -> IO ()
microNoteOn socket channel note vel =
  do pitchBendSet socket channel (round $ (note - (fromIntegral $ floor note))/pbu')
     noteOn       socket channel (floor note) vel

microNoteOff :: Socket -> Int -> Double -> IO ()
microNoteOff socket channel note =
  do send socket (C.pack $ "noteoff "++show channel++" "++show (floor note)++"\n")
     return ()

sequenceNotesFluid :: (TuningSystem note g, Show note)
   => Socket
   -> Int        -- Program number
   -> String     -- Soundfont name
   -> Score note -- [[note]]
   -> Int        -- Midi note velocity as volume
   -> Tempo
   -> Int -- Offset (for debug)
   -> Preformance
sequenceNotesFluid socket prog sft
  score vol bpm o = Preformance (do mapConcurrently go (zip score [0..n-1])
                                    return ())
  where n   = length score -- number of midi channels we need to use
        bps = bpm/60
        -- There's some strange scoping issue with this line
        -- I can't uncomment this for some reason
        -- run :: Line note -> Preformance
        go (line, i) = (forM_ line (\(n, duration) ->
                                           do setProgram  socket (i + o) prog   
                                              microNoteOn socket (i + o) (nfreq n) vol
                                              Vivid.wait $ (fromRational duration) * (1/bps)
                                              microNoteOff socket (i + o) (nfreq n)))

type Vol = Double
type Duration = Rational
data Inst = VividSynth (Vol -> SynthDef '["note"]) | FluidInst String Int
data Line' note = Line' Inst [(Maybe (note, Vol), Duration)]
type Score' note = [Line' note]

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

exscore1 = [Line' (VividSynth additiveSynth)
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

exscore2 = [Line' (VividSynth additiveSynth)
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
          {- go (l:ls) n =
            (preformLine socket l n tempo)
                     <> go ls (n+1)
          go [] n = mempty -}

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

sequenceLineFluid :: TuningSystem note g
  => Socket
  -> (String, Int) -- Fluid instrument data (Note, this might need to change, I'm not sure if
                   -- we can easily use more than one SF file at once
  -> [(Maybe (note, Vol), Rational)] -- Preformance data
  -> Int -- Channel number
  -> Tempo
  -> Preformance
sequenceLineFluid socket (sfpath, prog) line n bpm =
  Preformance (forM_ line (\(noteType, duration) ->
      case noteType of
           Just (note, vol) -> do putStrLn "Sequencing a fluid note"
                                  setProgram  socket n prog 
                                  microNoteOn socket n (nfreq note) (volToVel vol)
                                  Vivid.wait $ (fromRational duration) * (1/bps)
                                  putStrLn "Sequencing a fluid note (done waiting)"
                                  microNoteOff socket n (nfreq note)
           Nothing   -> do Vivid.wait $ (fromRational duration) * (1/bps)))
  where bps = (bpm/60) :: Double

sequenceLineVivid :: TuningSystem note g
   => (Double -> SynthDef '["note"])
   -> [(Maybe (note, Double), Rational)] -- Preformance data1
   -> Tempo
   -> Preformance
sequenceLineVivid syn line bpm = run line
  where bps = bpm/60
        run line = Preformance
            (forM_ line (\(noteType, duration) ->
                   case noteType of
                      Just (note, vol) -> do putStrLn "Sequencing a Vivid note"
                                             s <- synth (syn vol) (50::I "note")
                                             set s (toI (nfreq note) :: I "note")
                                             Vivid.wait $ (fromRational duration) * (1/bps)
                                             putStrLn "Sequencing a Vivid note (done waiting)"
                                             free s
                      Nothing -> do Vivid.wait $ (fromRational duration) * (1/bps)))

volToVel :: Double -> Int
-- Converts a volume message to a velocity
-- Note: Volumes should be a real number from 0 to 1
volToVel vol = round $ vol*127
                      

-- Note: This isn't really even needed anymore.
sequenceNotes :: TuningSystem note g
   => SynthDef '["note"]
   -> Score note
   -> Volume
   -> Tempo
   -> Preformance
sequenceNotes syn score vol bpm = foldr1 (<>) (map run score)
  where bps = bpm/60
        run line = Preformance (forM_ line (\(n, duration) ->
                                    do s <- synth syn (50::I "note")
                                       set s (toI (nfreq n) :: I "note")
                                       Vivid.wait $ (fromRational duration) * (1/bps)
                                       free s))
