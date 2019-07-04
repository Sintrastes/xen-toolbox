{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module QQTests where

import Parser22Eq
import Parser24Eq
import ParserBP
import ScalesGeneric

l1 :: Line Note22
l1 = [line22| AIs2 1 BQis2 1 |]

l2 :: Line Note24
l2 = [line24| AIs2 1, BQis2 1 |]

l3 :: Line NoteBP
l3 = [lineBP| A2 1, J2 1, HEs2 2, A2 1 |]
