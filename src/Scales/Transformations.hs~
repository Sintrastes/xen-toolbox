{-# LANGUAGE IncoherentInstances, AllowAmbiguousTypes, UndecidableInstances, FlexibleInstances, TypeSynonymInstances #-}

module Scales.Transformations where

import Data.Monoid.Action
import Data.Group
import Data.Semiring 
import Scales.Generic
import Control.Concurrent
import Vivid

-- Note: All this ``preformance'' stuff should probably go
-- in a new, dedicated file.
parallelThreads :: [IO ()] -> IO ()
parallelThreads threads = foldr1 parallelThreads2 threads

parallelThreads2 :: IO () -> IO () -> IO ()
parallelThreads2 t1 t2 = do tid <-forkIO t1
                            t2

newtype Preformance = Preformance { run :: IO () }
type Tempo = Double
type Volume = Double

instance Semigroup Preformance where
  (Preformance t1) <> (Preformance t2)
     = Preformance (parallelThreads2 t1 t2)
instance Monoid    Preformance where
  mempty = Preformance (wait 0)

instance TuningSystem note g => Semiring (Score note) where
-- This does not work for infinite lists
  -- Is there a better way to implement this?
  -- xs <.> ys = map (\(x,y) -> x ++ y) xys
  xs <.> ys = zipWith (++) xs ys
  --    where xys = zip xs ys
  -- [[]]    <.> ys      = ys
  -- xs      <.> [[]]    = xs
  -- []      <.> ys      = ys
  -- xs      <.> []      = xs
  -- (x:xs)  <.> ys = (x:y:(xs <.> ys))
  -- New approach:
  -- []      <.> rs     = rs
  -- ([]:ls) <.> (r:rs) = r:(ls <.> rs)
  -- (xs:ls) <.> (r:rs) = ((init xs):ls) <.> ( ((last xs):r) : rs)
  one = [[]]

-- Some examples of the semiring

cq :: (Note22, Rational)
cq = ((PitchClass22 C [], 4), 1)
eq :: (Note22, Rational)
eq = ((PitchClass22 E [], 4), 1)
gq :: (Note22, Rational)
gq = ((PitchClass22 G [], 4), 1)

line1 :: Score Note22
line1 = [[cq,eq,cq,eq,cq,eq,cq,eq]]
line2 :: Score Note22
line2 = [[gq,gq,gq,gq,gq,gq,gq,gq]]

inparallel = line1 <+> line2
insequence = line1 <.> line2

-- instance (TuningSystem note g) => Semiring (Score' note) where
--

-- transpose :: TuningSystem note g => g -> [note] -> [note]
-- Transpose a melody or a chord (represented by a list
-- of notes) by the intevral g
-- transpose g ns = map (act g) ns

-- inversion :: TuningSystem note g => [note] -> [note]
-- Invert a set of notes about the base note
-- inversion ns = map invert ns

relInversion :: TuningSystem note g => [g] -> [g]
-- Invert a scale (or chord) represented as a list
-- of integers about the tonic
relInversion gs = map invert gs
