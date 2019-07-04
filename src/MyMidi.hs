module MyMidi where

import System.MIDI
import System.MIDI.Base
import Vivid (wait)

-- Note and volume, respectively
-- -- NoteOn  60 126
-- --NoteOff 50 126
-- Maximum value
-- --PitchWheel 8191

-- Question: How do I change the midi instrument?
-- I think you have to do:
-- --ProgramChange 42

-- Note, all of these have type MidiMessage'

main = do
  sources <- enumerateSources
  dests   <- enumerateDestinations
  let channel = 0
  let patch = 2
  -- Open the first valid connection destination
  -- (for testing purposes)
  conn <- openDestination (dests !! 0)
  start conn
  -- do stuff
  send conn (MidiMessage channel (ProgramChange patch))
  send conn (MidiMessage channel (NoteOn 60 126))
  wait 40
  send conn (MidiMessage channel (NoteOff 60 126))
  stop conn
