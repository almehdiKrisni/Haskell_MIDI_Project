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


--Fonction permettant de jouer 16 menuets (recursive - joue 1 menuet par appel)
playMeasure :: GameOptions -> Integer -> IO ()
playMeasure opt x = do
  if x > 8
    then do
      --On joue une mesure de la première partie du menuet

      --Generation d'un index aléatoire
      --Thanks to the people on this website - https://en.wikibooks.org/wiki/Haskell/Libraries/Random
      gen <- newStdGen
      let ns = randoms gen :: [Int]
      let list = take 1 ns
      let len = length firstPart
      let (h:t) = map (`mod` len) list
      let index = (firstPart !! h)

      --On recupere la mesure liée à l'index
      let menuet = (measures !! (index - 1))

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
              -- putStrLn "\n\nStream is (playMeasure)"
              -- print stream
              
              --On modifie l'instrument utilisé
              changeInstrument instrumentChoice stream

              --On joue le menuet
              play menuet3 stream
              close stream
              
              --On joue le menuet suivant
              playMeasure opt (x - 1)
              return ()

    else
      if x > 0
        then do
          --On joue une mesure de la deuxième partie du menuet

          --Generation d'un index aléatoire
          --Thanks to the people on this website - https://en.wikibooks.org/wiki/Haskell/Libraries/Random
          gen <- newStdGen
          let ns = randoms gen :: [Int]
          let list = take 1 ns
          let len = length secondPart
          let (h:t) = map (`mod` len) list
          let index = (secondPart !! h)

          --On recupere la mesure liée à l'index
          let menuet = (measures !! (index - 1))

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
                  -- putStrLn "\n\nStream is (playMeasure)"
                  -- print stream
                  
                  --On modifie l'instrument utilisé
                  changeInstrument instrumentChoice stream

                  --On joue le menuet
                  play menuet3 stream
                  close stream
                  
                  --On joue le menuet suivant
                  playMeasure opt (x - 1)
                  return ()

      else
        --Fin du menuet

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
      putStrLn "On joue le menuet ..."
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
