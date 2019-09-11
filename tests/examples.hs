{-# LANGUAGE DataKinds, ExtendedDefaultRules, FlexibleContexts, TemplateHaskell, QuasiQuotes #-}
-- Copyright 2014 Google Inc. All rights reserved.
--
-- Licensed under the Apache License, Version 2.0 (the "License"); you may not
-- use this file except in compliance with the License. You may obtain a copy of
-- the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
-- WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
-- License for the specific language governing permissions and limitations under
-- the License.

import Vivid
import IO.VividPlaybackUtils
import qualified Data.ByteString.Char8 as C
import Network.Simple.TCP
import Scales.Generic
import Control.Concurrent.Async
import Scales.Transformations
import Data.Semiring
import Parsers.Eq22
import Parsers.BP

main = do
      
     a1 <- async $ connect "localhost" "9988" $ \(socket1, addr1) ->
               do loadSoundFont socket1 "test.sf3"
                  Vivid.wait 3
                  putStrLn "Playing Bohlen Pierce example score..."
                  a <- async $ do
                     run $ sequenceNotes' socket1 exBPScore 80
                     putStrLn "Done playing BP example score."
                  Control.Concurrent.Async.wait a
     Control.Concurrent.Async.wait a1
     
      
     a2 <- async $ connect "localhost" "9988" $ \(socket1, addr1) ->
                 do loadSoundFont socket1 "test.sf3" 
                    putStrLn "Playing simple 22edo example score..."
                    run $ sequenceNotes' socket1 exscore1 120
                    putStrLn "Done playing simple 22edo example score."
     Control.Concurrent.Async.wait a2
    
     a3 <- async $ connect "localhost" "9988" $ \(socket1, addr1) ->
                 do loadSoundFont socket1 "test.sf3" 
                    putStrLn "Playing 22edo example score combining supercollider and fluidsynth outputs..."
                    let tempo = 100
                        -- A simple two-line motif which we will play on the electric piano
                        motif1 = [ [line22| C2 2, C2 2, C2 2, C2 2 |] :: Line Note22,
                                   [line22| E4 2, G4 2, E4 2, GEs4 2%3, GEsQes4 2%3, GEsQesQes4 2%3 |]]
                        -- A simple motif moving between two chords which we will play on an organ
                        motif2 = [[line22| E4 6, F4 2 |] :: Line Note22,
                                  [line22| G3 6, G3 2 |],
                                  [line22| B3 6, C4 2 |],
                                  [line22| B4 8 |]]
                    run $
                      (sequenceNotesFluid socket1 4 "test.sf3"
                       -- insequence
                       (motif1 <.> motif1)
                       tempo
                       70
                       0)
                  --   -- I think maybe the server gets confused (race condition?)
                  --   -- when I try to send too much on one connection?
                  --   -- (I don't actually think this is an issue at this point)
                  --   -- The issue comes when we repeat -- so this should be
                  --   -- handled internally
                      <> (sequenceNotesFluid socket1 17 "test.sf3"
                        (motif2 <.> motif2)
                         tempo
                         70
                         2)
                    Vivid.wait 1
     Control.Concurrent.Async.wait a3       

