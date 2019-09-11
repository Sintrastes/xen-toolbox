
module IO.Vivid.Sequencer

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
