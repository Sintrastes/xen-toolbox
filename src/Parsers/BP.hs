{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Parsers.BP where

import Text.ParserCombinators.Parsec
import Text.Parsec.Expr
import Scales.Generic
import Data.List.Split hiding (oneOf, sepBy)
import Control.Monad
import Data.Ratio
import Text.Read
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- Example TemplateHaskell usage:
--
-- line = [line| AIs2 1, A2 1/2, AIs2 1/2, AQis 1/2,
--               CQes3 1/5, D3 1/5, EQis3 2/5, DSes3 1/5 ] 
--
-- lines = [lines| C2 4,                   E2 4,                ;
--                 C3 1, C3 1, D3 1, E3 1, G3 1, G3 1, A3 1, B3 1 ]
--
--
-- Note: We could also have notation for interval based input, e.x.
--
-- melody = [melodya | U 1,    M2 1,     Mi2 1,
--                     U 1,    M2 1,     Mi2 1,
--                     U 1, Sm2 2/3, M2 2/3, Mi2 2/3]
--
-- which uses absolute placement of the notes from the tonic, as
-- opposed to:
--
-- melody = [ melodyr | U1, M2 1, M2 1, Mi2 1, M2 1, M2 1, M2 1, Mi2 4 ]
--
-- Note also that this notation isn't really nescesary for either relative
-- melodies by scale degree, or absolute melodies by scale degree,
-- as these can be viewed simply as lists of integers.
--
-- However, we should provide utility functions for converting between
-- each of these cases.
--

-- intervals22ToG :: String -> Maybe Edo22
-- intervals22ToG s = case s of
--                      "u" -> Just 
--                      "" -> Just 
--                      " " -> Just 
--                      otherwise -> Nothing

parseLineBPQQ :: String -> Q Exp
parseLineBPQQ s =
  case parse noteLineBP "" s of
    Left  err    -> fail (show err)
    Right x      -> [e| x |]

lineBP :: QuasiQuoter
lineBP = QuasiQuoter {
    quoteExp  = parseLineBPQQ
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }
  where notHandled things = error $
            things ++ " are not handled by the regex quasiquoter."

noteLinesBP :: GenParser Char st [[(NoteBP, Rational)]]
noteLinesBP = do
  ls <- noteLineBP `sepBy` many (oneOf " \n;")
  eof
  return ls

noteLineBP :: GenParser Char st [(NoteBP, Rational)]
noteLineBP = do
  _     <- spaces
  ndurs <- parseNoteDur `sepBy` many (oneOf " ,")
  return ndurs
  where parseNoteDur :: GenParser Char st (NoteBP, Rational)
        parseNoteDur = do
          note <- noteName
          _    <- spaces
          dur  <- duration
          _    <- spaces
          return (note, dur)

noteName :: GenParser Char st NoteBP
noteName = do
  note_head <- oneOf "ABCDEFGHJ"
  as        <- accidentals
  n         <- oneOf "0123456789"
  let note = case note_head of
                'A' -> Ab
                'B' -> Bb
                'C' -> Cb
                'D' -> Db
                'E' -> Eb
                'F' -> Fb
                'G' -> Gb
                'H' -> Hb
                'J' -> Jb
                otherwise -> undefined
  return (PitchClassBP note as, read [n] :: Int)

duration :: GenParser Char st Rational
duration = do
  string <- many $ noneOf " ,"
  let d = readMaybe string :: Maybe Rational
  let d' = read string :: Int
  case d of
    Just x  -> return x
    Nothing -> return (fromIntegral d')

duration' :: GenParser Char st Rational
duration' = do
  string <- many $ noneOf " "
  let d = read string
  return d

accidentals :: GenParser Char st [StdAccidental]
accidentals = do
  string <- many (noneOf "\n 1234567890/,")
  let identifiers = map (++"s") $ init (splitOn "s" string)
  let parsed = map (\x -> case x of
          "Is"  -> Sharp
          "Es"  -> Flat
          otherwise -> undefined) identifiers
  return parsed
