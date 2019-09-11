
module IO.Fluidsynth.Util where

setProgram :: Socket -> Int -> Int -> IO ()
setProgram socket channel prog = send socket (C.pack $ "prog "++show channel++" "++show prog++"\n")

openServer :: Int -> Int -> IO ()
openServer channels port = do
   system $ "fluidsynth -K "++ show channels ++" -j  -i -s -o \"shell.port="++show port++"\""
   return ()
   
loadSoundFont :: Socket -> String -> IO ()
loadSoundFont socket filepath = send socket (C.pack $ "load "++show filepath++"\n")

noteOn :: Socket -> Int -> Int -> Int -> IO ()
noteOn socket channel note vel = send socket (C.pack $ "noteon "++show channel++" "++show note++" "++show vel++"\n")

noteOff :: Socket -> Int -> Int -> IO ()
noteOff socket channel note = send socket (C.pack $ "noteoff "++show channel++" "++show note++"\n")

pitchBendSet :: Socket -> Int -> Int -> IO ()
pitchBendSet socket channel value = send socket (C.pack $ "pitch_bend "++show channel++" "++show value++"\n")
  
microNoteOn :: Socket -> Int -> Double -> Int -> IO ()
microNoteOn socket channel note vel =
  do pitchBendSet socket channel (round $ (note - (fromIntegral $ floor note))/pbu')
     noteOn       socket channel (floor note) vel

microNoteOff :: Socket -> Int -> Double -> IO ()
microNoteOff socket channel note =
  do send socket (C.pack $ "noteoff "++show channel++" "++show (floor note)++"\n")
     return ()

