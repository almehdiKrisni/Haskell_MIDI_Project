module Midi where

import Data.List
import Sound.PortMidi

collectMidiNote :: Integer -> Integer -> Integer -> Integer -> [(Integer, PMMsg)]
collectMidiNote h d v at =
  let noteOn = PMMsg 0x90 (fromIntegral $ h) (fromIntegral $ v)
      noteOff = PMMsg 0x90 (fromIntegral $ h) (fromIntegral $ 0)
   in [(at, noteOn), ((at + d), noteOff)]

myPredicate (a1, a2) (b1, b2) = compare a1 b1

sortMidi :: [(Integer, PMMsg)] -> [(Integer, PMMsg)]
sortMidi l = sortBy myPredicate l

changeInstrument :: Integer -> PMStream -> IO ()
changeInstrument num stream = do
  -- putStrLn "Stream is (changeInstrument)"
  -- print stream
  startTime <- time
  let pgmchange = PMMsg 0xC0 (fromIntegral $ num) (fromIntegral $ 0)
      evt1 = PMEvent (encodeMsg pgmchange) startTime
  writeShort stream evt1
  return ()