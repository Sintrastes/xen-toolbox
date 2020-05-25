
module IO.Fluidsynth.Sequencer where

import IO.Fluidsynth.Util

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
