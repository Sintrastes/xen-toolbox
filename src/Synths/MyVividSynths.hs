{-# LANGUAGE TemplateHaskell, DataKinds, ExtendedDefaultRules #-}

module Synths.MyVividSynths where

import Vivid

wobbleSound :: Double -> SynthDef '["note"]
wobbleSound vol = sd (0 ::I "note") $ do
   s <- 50 ~* sinOsc (freq_ 10) ? KR
   freq <- midiCPS (V::V "note") ~+ s
   s1 <- sinOsc (freq_ freq)
   s2 <- vol ~* 0.1 ~* s1
   out 0 [s2, s2]

tone :: Double -> SynthDef '["note"]
tone vol = sd (0::I "note") $ do
   a <- lfTri (freq_ 0.2) ? KR ~* 0.5 ~+ 0.5
   freq <- lag (in_ $ midiCPS (V::V "note"), lagSecs_ 1.25) ? KR
   b <- 0.1 ~* vol ~* varSaw (freq_ freq, width_ a)
   out 0 [b, b]

triangle :: Double -> SynthDef '["note"]
triangle vol = sd (0::I "note") $ do
  a <- 0.1 ~* vol ~* lpf (freq_ 4000.0, in_ $ lfTri (freq_ $ midiCPS (V::V "note")))
  out 0 [a,a]

noisyTriangle :: Double -> SynthDef '["note"]
noisyTriangle vol = sd (0::I "note") $ do
  a <- vol ~* (0.01 ~* brownNoise) ~+  (0.1 ~* lpf (freq_ 4000.0, in_ $ lfTri (freq_ $ midiCPS (V::V "note"))))
  out 0 [a,a]

theSound :: Double -> SynthDef '["note"]
theSound vol = sd (0 ::I "note") $ do
   wobble <- sinOsc (freq_ 0.5) ? KR ~* 10 ~+ 10
   s <- vol ~* 0.1 ~* sinOsc (freq_ $ midiCPS (V::V "note") ~+ wobble)
   out 0 [s,s]
  
simpleSineWave :: Double -> SynthDef '["note"]
simpleSineWave vol = sd (0 ::I "note") $ do
  s <- 0.1 ~* sinOsc (freq_ $ midiCPS (V::V "note"))
  out 0 [s,s]

additiveSynth :: Double -> SynthDef '["note"]
additiveSynth vol = sd (0 ::I "note") $ do
  p0 <- v ~* sinOsc (freq_ $ midiCPS (V::V "note"))
  p1 <- (v*0.4) ~* sinOsc (freq_ $ 2 ~* midiCPS (V::V "note"))
  p2 <- (v*0.3) ~* sinOsc (freq_ $ 3 ~* midiCPS (V::V "note"))
  p3 <- (v*0.3) ~* sinOsc (freq_ $ 4 ~* midiCPS (V::V "note"))
  p4 <- (v*0.2) ~* sinOsc (freq_ $ 5 ~* midiCPS (V::V "note"))
  p5 <- (v*0.1) ~* sinOsc (freq_ $ 6 ~* midiCPS (V::V "note"))
  p6 <- (v*0.01) ~* sinOsc (freq_ $ 7 ~* midiCPS (V::V "note"))
  p7 <- (v*0.2) ~* sinOsc (freq_ $ 8 ~* midiCPS (V::V "note"))
  p8 <- (v*0.04) ~* sinOsc (freq_ $ 9 ~* midiCPS (V::V "note"))
  let s = (p0 ~+ p2 ~+ p3 ~+ p4 ~+ p5 ~+ p6 ~+ p7 ~+ p8)
             ~* line (start_ 0, end_ 1)
  out 0 [s,s]
  where v = 0.08


makeAddSynth :: Int -- Number of partials
             -> Double   -- base volume
             -> [Double] -- volume of each one of the particles
             -> Maybe [Double] -- (optional) tuning of the partials
                               -- in frequency derivations from the
                               -- harmonic series
             -> Double -- Volume control
             -> SynthDef '["note"]
-- Make an additive synth with no modifications of the partials
-- from the harmonic series
makeAddSynth n vol vs Nothing v = sd (0 :: I "note") $ do
    let ps = map (\(v,k) -> (v * vol) ~* sinOsc (freq_ $ k ~* midiCPS (V::V "note")))
         (zip vs [1..n])
    let s = (foldr1 (~+) ps)
               ~* line (start_ 0, end_ 1) -- fade in envelope
               ~* line (start_ 1, end_ 0) -- fade out envelope
    out 0 [s,s]
-- Make an additive synth with modified (inharmonic) partials
makeAddSynth n vol vs (Just pmods) v = sd (0 :: I "note") $ do
    let ps = map (\(v,k,p) -> (v * vol) ~* sinOsc (freq_ $ (p * (fromIntegral k)) ~* midiCPS (V::V "note")))
         (zip3 vs [1..n] pmods)
    let s = (foldr1 (~+) ps) ~* line (start_ 0, end_ 1)
    out 0 [s,s]
