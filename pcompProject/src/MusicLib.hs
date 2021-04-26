module MusicLib where
import Control.Monad.State

import Midi
import Sound.PortMidi
import Control.Concurrent (threadDelay)
-- Music interface

data MusObj = Note  Integer Integer Integer | 
              Chord Integer [MusObj] | 
              Measure [MusObj] deriving (Show) 

--Calcule la duree d'un objet musical
getOnset :: MusObj -> Integer
getOnset (Note p d v) = 0
getOnset (Chord onset elems) = onset
getOnset (Measure elems) = 0

getDur :: MusObj -> Integer
getDur (Note p d v) = d
getDur (Chord onset elems) = foldl max 0 (map getDur elems)
getDur (Measure elems) = foldl max 0 (map (\x -> (getDur x) + (getOnset x)) elems)

collectMidi :: MusObj -> Integer -> [(Integer, PMMsg)]
collectMidi (Note h d v) at = collectMidiNote h d v at
collectMidi _ at = [] -- a vous de faire

play :: MusObj -> PMStream -> IO ()
play obj stream = do
  startTime <- time
  let dur = getDur obj
  let midiEvents = (sortMidi (collectMidi obj 0))
  let evts =
        map
          ( \(t, msg) ->
              PMEvent
                { message = (encodeMsg msg),
                  timestamp = (fromIntegral $ t) + startTime
                }
          )
          midiEvents
  writeEvents stream evts
  threadDelay (fromIntegral $ (dur * 1000))
  return ()

--Renvoie le nombre de notes d'un objet musical
countNotes :: MusObj -> Int
countNotes (Note p d v) = 1
countNotes (Chord onset l) = length l 
countNotes (Measure l) = sum (map countNotes l)

--Retourne un nouvel objet musical dont la durée a été multipliée par un facteur flottant
stretch :: MusObj -> Float -> MusObj 
stretch (Note p d v) f = Note p x v where
  x = round (fromIntegral d * f)
stretch (Chord onset elems) f = Chord onset (map helper elems) where
  helper = (`stretch` f)
stretch (Measure elems) f = Measure (map helper elems) where
  helper = (`stretch` f)

--Retourne un nouvel objet musical dont les hauteurs ont été additionées de n demitons
transpose :: MusObj -> Integer -> MusObj
transpose (Note p d v) n = Note (p + n) d v
transpose (Chord onset elems) n = Chord onset (map helper elems) where
  helper = (`transpose` n)
transpose (Measure elems) n = Measure (map helper elems) where
  helper = (`transpose` n)

--Fait le miroir des toutes les hauteurs d’un objet musical autour d’une hauteur donnée.
mirror :: MusObj -> Integer -> MusObj
mirror (Note p d v) c = Note (c - (p - c)) d v
mirror (Chord onset elems) c = Chord onset (map helper elems) where
  helper = (`mirror` c)
mirror (Measure elems) c = Measure (map helper elems) where
  helper = (`mirror` c)

