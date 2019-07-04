{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Parsers.Eq22 where

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

parseLine22QQ :: String -> Q Exp
parseLine22QQ s =
  case parse noteLine22 "" s of
    Left  err    -> fail (show err)
    Right x      -> [e| x |]

line22 :: QuasiQuoter
line22 = QuasiQuoter {
    quoteExp  = parseLine22QQ
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }
  where notHandled things = error $
            things ++ " are not handled by the regex quasiquoter."

noteLines22 :: GenParser Char st [[(Note22, Rational)]]
noteLines22 = do
  ls <- noteLine22 `sepBy` many (oneOf " \n;")
  eof
  return ls

noteLine22 :: GenParser Char st [(Note22, Rational)]
noteLine22 = do
  _     <- spaces
  ndurs <- parseNoteDur `sepBy` many (oneOf " ,")
  return ndurs
  where parseNoteDur :: GenParser Char st (Note22, Rational)
        parseNoteDur = do
          note <- noteName
          _    <- spaces
          dur  <- duration
          _    <- spaces
          return (note, dur)

noteName :: GenParser Char st Note22
noteName = do
  note_head <- oneOf "ABCDEFG"
  as        <- accidentals
  n         <- oneOf "0123456789"
  let note = case note_head of
                'A' -> A
                'B' -> B
                'C' -> C
                'D' -> D
                'E' -> E
                'F' -> F
                'G' -> G
                otherwise -> undefined
  return (PitchClass22 note as, read [n] :: Int)

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

accidentals :: GenParser Char st [QuarterAccidental]
accidentals = do
  string <- many (noneOf "\n 1234567890/,")
  let identifiers = map (++"s") $ init (splitOn "s" string)
  let parsed = map (\x -> case x of
          "Qis" -> Qis 
          "Is"  -> Is
          "Sis" -> Sis
          "Qes" -> Qes
          "Es"  -> Es
          "Ses" -> Ses
          otherwise -> undefined) identifiers
  return parsed
