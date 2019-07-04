{-# LANGUAGE MultiParamTypeClasses, KindSignatures, TypeOperators, TypeFamilies, FlexibleInstances, DataKinds, ExtendedDefaultRules, GADTs, AllowAmbiguousTypes, TypeInType, ScopedTypeVariables #-}

module MyVivid where

-- to avoid name pollution with Data.Category
import Prelude hiding ((.))
-- import qualified Prelude ((.))
import Data.Monoid
import Data.Kind
import Vivid
import Data.Ratio
import Control.Concurrent
import qualified Music.Lilypond as LP
import Data.Semiring
-- import Data.Type.Natural
-- import GHC.TypeLits

data Nat = Z | S Nat

data SNat n where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

snatToInt :: SNat n -> Int
snatToInt SZ = 0
snatToInt (SS n) = 1 + snatToInt n

natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

nlit :: Int -> Nat
nlit 0 = Z
nlit n
  | n > 0 = S (nlit (n-1))
  | n < 0 = undefined

instance Show Nat where
  show n = show (natToInt n)

newtype NoteSequenceCat (n :: Nat) = NoteSequenceCat NoteSequence

newtype Transformation (n :: Nat) (m :: Nat) =
  Transformation ((NoteSequenceCat n) -> (NoteSequenceCat m))

-- This works if I allow ambigious types, but I'm not sure if this is what I want here
-- class MyCategoryClass (obj :: ) hom where
  -- data Obj a :: Type
  -- hom :: Obj a -> Obj a -> Type

-- This reqires TypeInType?
-- No, this doesn't even compile with that.
-- I think this is too far in the dependent type territory to work.
-- class MyCategoryClass obj where
--   hom     :: obj -> obj -> Type
--   id (a :: obj) :: hom a a
-- instance MyCategoryClass N

class Category (obj :: Type) (hom :: obj -> obj -> Type) where
    id   :: hom a a
    (.) :: hom b c -> hom a b -> hom a c 

instance Category Nat Transformation where
   id = Transformation (\x -> x)
   (Transformation g) . (Transformation f) = Transformation (g . f)

instance Category Type (->) where
  id = \x -> x
  g . f = (.) g f

class Category obj hom
   => MonoidalCategory (obj :: Type)
         (hom   :: obj -> obj -> Type)
         (munit :: Type) where
            tens :: obj -> obj -> obj
            tensf :: hom a b -> hom c d -> hom (a `tens` c) (b `tens` d)

ttens :: Nat -> Nat -> Nat
-- Note: Apparently, to actualy work with the type literals in GHC.TypeLits
-- you have to do all sorts of wizardry -- so I might want to use my own
-- "nat" type family here instead.
-- The other option is that I could define it myself
ttens n m = nlit ((natToInt n) + (natToInt m))

splitSeq :: SNat n ->  NoteSequenceCat (ttens n m) -> (NoteSequenceCat n, NoteSequenceCat m)
splitSeq n (NoteSequenceCat xs) = (NoteSequenceCat as, NoteSequenceCat bs)
  where (as, bs) = splitAt n xs

ttensf :: Transformation (n :: Nat) (m :: Nat)
       -> Transformation (k :: Nat) (l :: Nat)
       -> Transformation (ttens n k :: Nat) (ttens m l :: Nat)
ttensf (Transformation f) (Transformation g) =
  Transformation (\x -> let (as, bs) = splitSeq n x in f as)

test :: NoteSequenceCat (S (S Z))
test = NoteSequenceCat
    [ [((PitchClass C [], 4), 2), ((PitchClass C [],   4), 2),
       ((PitchClass C [], 3), 2), ((PitchClass C [],   3), 2)]
    , [((PitchClass E [], 4), 2), ((PitchClass G [],   4), 2),
       ((PitchClass E [], 4), 2), ((PitchClass G [Es], 4), 2/3),
       ((PitchClass G [Es,Qes], 4), 2/3), ((PitchClass G [Es,Qes,Qes], 4), 2/3) ]]
