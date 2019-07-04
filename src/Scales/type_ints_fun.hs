{-# LANGUAGE DataKinds, KindSignatures #-}

import GHC.TypeLits

newtype NoteSequence (n :: Nat) a = NoteSequence a

x = NoteSequence [["a","b","c","d"]] :: NoteSequence 1 [[String]]
