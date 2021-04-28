module Main where

import Control.Concurrent
import Sound.PortMidi
import Midi
import MusicLib
import Control.Monad.State
import DataBase
import System.Random

--Permet de connaitre les informations à propos d'un device de sortie
midiDevicePrint :: Int -> IO ()
midiDevicePrint 0 = getDeviceInfo 0 >>= print
midiDevicePrint n = do
  getDeviceInfo n >>= print
  midiDevicePrint (n - 1)

--New Options Monad
data GameOptions = GameOptions
  { deviceSelect :: DeviceID
  , instrumentNumber :: Integer
  , transpositionMode :: Integer
  , mirrorMode :: Integer
  , speedFactor :: Float}

--Modifier le device de sortie
modifyOutputDevice :: DeviceID -> State GameOptions DeviceID
modifyOutputDevice newVal = do
      options <- get

      put $ options { deviceSelect = newVal }
      return (newVal)


--Modifier le numéro de l'instrument
modifyInstrumentNumber :: Integer -> State GameOptions Integer
modifyInstrumentNumber newVal = do
  options <- get

  put $ options { instrumentNumber = newVal }
  return (newVal)


--Modifier le mode de transposition
modifyTranspositionMode :: Integer -> State GameOptions Integer
modifyTranspositionMode newVal = do
  options <- get

  put $ options { transpositionMode = newVal }
  return (newVal)


--Modifier le mode miroir
modifyMirrorMode :: Integer -> State GameOptions Integer
modifyMirrorMode newVal = do
  options <- get

  put $ options { mirrorMode = newVal }
  return (newVal)


--Modifier le facteur de vitesse
modifyMusicSpeed :: Float -> State GameOptions Float
modifyMusicSpeed newVal = do
  options <- get

  put $ options { speedFactor = newVal }
  return (newVal)


testMeasure =  Measure [
            (Chord 0 [(Note 42 610 86),(Note 54 594 81),(Note 81 315 96)]),
            (Chord 292 [(Note 78 370 78)]),
            (Chord 601 [(Note 76 300 91),(Note 43 585 83),(Note 55 588 98)]),
            (Chord 910 [(Note 79 335 96)]),
            (Chord 1189 [(Note 73 342 86),(Note 57 595 76),(Note 45 607 83)]),
            (Chord 1509 [(Note 76 280 93)])]


testMeasure2 = [
  Measure [
            (Chord 0 [(Note 42 610 86),(Note 54 594 81),(Note 81 315 96)]),
            (Chord 292 [(Note 78 370 78)]),
            (Chord 601 [(Note 76 300 91),(Note 43 585 83),(Note 55 588 98)]),
            (Chord 910 [(Note 79 335 96)]),
            (Chord 1189 [(Note 73 342 86),(Note 57 595 76),(Note 45 607 83)]),
            (Chord 1509 [(Note 76 280 93)])],
   Measure [
            (Chord 0 [(Note 49 616 86)]),
            (Chord 295 [(Note 64 314 78)]),
            (Chord 583 [(Note 52 616 88),(Note 68 296 81)]),
            (Chord 863 [(Note 69 290 96)]),
            (Chord 1168 [(Note 57 607 79),(Note 73 305 79)]),
            (Chord 1473 [(Note 76 310 100)])],
   Measure [
            (Chord 0 [(Note 50 596 91),(Note 66 575 108),(Note 74 588 108)]),
            (Chord 612 [(Note 54 297 76),(Note 66 576 85),(Note 74 576 85)]),
            (Chord 901 [(Note 57 305 78)]),
            (Chord 1181 [(Note 66 575 69),(Note 74 575 69),(Note 62 610 86)])],
   Measure [
            (Chord 0 [(Note 76 447 105),(Note 43 636 86),(Note 55 616 85)]),
            (Chord 435 [(Note 71 150 69)]),
            (Chord 595 [(Note 42 600 108),(Note 54 600 108),(Note 69 300 108)]),
            (Chord 995 [(Note 71 150 69)]),
            (Chord 1197 [(Note 67 538 108),(Note 40 567 93),(Note 52 588 102)])],
   Measure [
            (Chord 0 [(Note 45 374 96),(Note 69 1787 108)]),
            (Chord 286 [(Note 49 360 86)]),
            (Chord 574 [(Note 52 317 88)]),
            (Chord 864 [(Note 57 340 85)]),
            (Chord 1156 [(Note 45 628 86)])]]


--Fonction permettant de jouer 16 menuets (recursive - joue 1 menuet par appel)
playMeasure :: GameOptions -> Integer -> IO ()
playMeasure opt x = do
  if x > 0
    then do
      --Generation d'un index aléatoire
      --Thanks to the people on this website - https://en.wikibooks.org/wiki/Haskell/Libraries/Random
      gen <- newStdGen
      let ns = randoms gen :: [Int]
      let list = take 1 ns
      let len = length measures
      let (h:t) = map (`mod` len) list

      --On recupere la mesure liée à l'index
      let menuet = (measures !! h)

      --On adapte la mesure
      --On recupere les parametres
      let instrumentChoice = instrumentNumber opt
      let transpoModeSelected = transpositionMode opt
      let mirrorModeSelected = mirrorMode opt
      let speedFactorSelected = speedFactor opt

      --On adapte le mode de transposition
      let menuet1 = transpose menuet transpoModeSelected

      --On adapte le mode miroir
      let menuet2 = mirror menuet1 mirrorModeSelected 

      --On adapte la vitesse d'execution
      let menuet3 = stretch menuet2 speedFactorSelected

      --On prend l'output device par defaut
      deviceId <- getDefaultOutputDeviceID

      case deviceId of
        Nothing -> putStrLn "Pas de port Midi par default"

        Just n -> do
          result <- openOutput n 1
          case result of
            Left err -> do
              putStrLn "Erreur lors de la lecture du menuet.\n"
              return ()

            Right stream -> do
              --On modifie l'instrument utilisé
              changeInstrument instrumentChoice stream

              --On joue le menuet
              play menuet3 stream
              close stream
              
              --On joue le menuet suivant
              playMeasure opt (x - 1)
              return ()

    else do
      return ()


--Fonction permettant de créer le Menu
menu :: GameOptions -> IO ()
menu opt = do

  --Affichage principal du menu
  putStrLn "\nMenu Principal - Le Jeu de Mozart\n1 - Jouer un menuet\n2 - Modifier le device de sortie\n3 - Modifier l'instrument\n4 - Modifier le mode de transposition\n5 - Modifier le mode miroir\n6 - Modifier la vitesse\n7 - Reinitialiser les paramètres de jeu\n0 - Fermeture du menu\n\nVeuillez choisir une option :"

  --On chosit une option
  choice <- getLine
  case choice of

    --Choice = 1
    "1" -> do
      --On joue un menuet
      terminate
      initialize  

      --On appelle la fonction allant jouer 16 mesures
      playMeasure opt 16
      menu opt
      return ()            

    --Choice = 2
    "2" -> do
      --On print les drivers
      terminate
      initialize

      n <- countDevices
      midiDevicePrint (n - 1)

      --On choisit le driver
      putStrLn "\nQuel device souhaitez-vous utiliser ?"
      choice2 <- getLine
      let deviceIdInt = read choice2 :: Int
      let deviceId = read choice2 :: DeviceID
      result <- openOutput deviceIdInt 1

      --On vérifie si le port existe
      case result of
        Left err -> do
          putStrLn "Ce portMidi n'existe pas ou n'est pas compatible."
          menu opt
          return ()

        Right stream -> do
          putStrLn "Modification du portMidi en cours ..."
          let (newOpt, optModified) = runState (modifyOutputDevice deviceId) opt
          menu optModified
          return ()

    --Choice = 3
    "3" -> do

      --On choisit l'instrument à utiliser
      putStrLn "\nQuel instrument souhaitez-vous utiliser ?"
      choice <- getLine
      let choiceInt = read choice :: Integer
      if choiceInt > 0 && choiceInt < 6
        then do
          putStrLn "Modification de l'instrument en cours ..." 
          let (newOpt, optModified) = runState (modifyInstrumentNumber choiceInt) opt
          menu optModified
          return()

        else do
          putStrLn "L'instrument n'existe pas."
          menu opt
          return()

      

    --Choice = 4
    "4" -> do
      putStrLn "\nQuel mode de transposition souhaitez-vous ?\n0 = Sans transposition\n1 = Plus 12 demitons\n2 = Moins 12 demitons"
      choice <- getLine
      let choiceInt = read choice :: Integer
      if choiceInt > -1 && choiceInt < 3
        then do
          case choiceInt of
            0 -> do
              putStrLn "Modification du mode de transposition en cours ..."
              let (newOpt, optModified) = runState (modifyTranspositionMode 0) opt
              menu optModified
              return ()

            1 -> do
              putStrLn "Modification du mode de transposition en cours ..."
              let (newOpt, optModified) = runState (modifyTranspositionMode 12) opt
              menu optModified
              return ()
            
            2 -> do
              putStrLn "Modification du mode de transposition en cours ..."
              let (newOpt, optModified) = runState (modifyTranspositionMode (- 12)) opt
              menu optModified
              return ()

        else do
          putStrLn "Ce mode de transposition n'existe pas."
          menu opt
          return ()

    --Choice = 5
    "5" -> do
      putStrLn "\nSouhaitez-vous activer le mode miroir ?\n0 = Mode miroir désactivé\n1 = Miroir activé à 100"
      choice <- getLine
      let choiceInt = read choice :: Integer
      if choiceInt > -1 && choiceInt < 2
        then do
          case choiceInt of
            0 -> do
              putStrLn "Modification du mode miroir en cours ..."
              let (newOpt, optModified) = runState (modifyMirrorMode 0) opt
              menu optModified
              return ()

            1 -> do
              putStrLn "Modification du mode miroir en cours ..."
              let (newOpt, optModified) = runState (modifyMirrorMode 100) opt
              menu optModified
              return ()

        else do
          putStrLn "Ce mode miroir n'existe pas."
          menu opt
          return ()

    --Choice = 6
    "6" -> do
      putStrLn "\nQuelle vitesse d'execution souhaitez-vous ?\n(Valeur comprise dans [0.0 , 1000.0] - Plus la valeur est faible, plus la musique est rapide !)"
      choice <- getLine
      let choiceFloat = read choice :: Float
      if choiceFloat > 0.0 && choiceFloat < 1000.0 --Vitesse limitée à x1000
        then do
          putStrLn "Modification de la vitesse d'execution en cours ..."
          let (newOpt, optModified) = runState (modifyMusicSpeed choiceFloat) opt
          menu optModified
          return ()

        else do
          putStrLn "Cette vitesse d'execution est impossible."
          menu opt
          return ()

    --Choice = 7
    "7" -> do
      putStrLn "Réinitialisation en cours ..."
      newOpt <- return (GameOptions 0 1 0 0 1.0)
      menu newOpt
      return ()

    --Choice = 0
    "0" -> do
      putStrLn "\nFermeture du menu. Au revoir.\n"
      return ()

--Main
main :: IO ()
main = do
  putStrLn "Le Jeu de Mozart"

  --Options par défaut
  opt <- return (GameOptions 0 1 0 0 1.0)

  --On ouvre le menu
  menu opt
  terminate
  return ()
