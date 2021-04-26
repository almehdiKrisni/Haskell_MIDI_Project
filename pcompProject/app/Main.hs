module Main where

import Control.Concurrent
import Sound.PortMidi
import Midi
import MusicLib
import Control.Monad.State

--Permet de connaitre les informations à propos d'un device de sortie
midiDevicePrint :: Int -> IO ()
midiDevicePrint 0 = getDeviceInfo 0 >>= print
midiDevicePrint n = do
  getDeviceInfo n >>= print
  midiDevicePrint (n - 1)



--Options 
--Positions des valeurs
--Options[0] => device de sortie
--Options[1] => numero de l'instrument
--Options[2] => mode de transposition
--Options[3] => mode miroir
--Options[4] => facteur f de multiplication de la durée

type Options = (Integer, Integer, Integer, Integer, Float)
-- Fixer le dernier Integer pour pouvoir mettre un float

--Cette version de playGame ne permet pas de modifier le dernier paramètre
modifyParameters :: (String, String) -> State Options Options
modifyParameters (x, y) = do 
  (op1, op2, op3, op4, op5) <- get 

  --On vérifie le choix
  let n = read y :: Integer
  let n2 = read y :: Float

  case x of
    --On modifie la valeur du driver de sortie
    "0" -> if n > 0 then put (n, op2, op3, op4, op5) --La valeur du driver de sortie ne peut pas etre négative
      else put (op1, op2, op3, op4, op5)
    
    -- On modifie le numéro de l'instrument choisi
    "1" -> case n of
      1 -> put (op1, 1, op3, op4, op5) 
      2 -> put (op1, 2, op3, op4, op5)
      3 -> put (op1, 3, op3, op4, op5)
      4 -> put (op1, 4, op3, op4, op5)
      5 -> put (op1, 5, op3, op4, op5)
      _ -> put (op1, op2, op3, op4, op5)

    --On modifie le mode de transposition
    "2" -> case n of
      0 -> put (op1, op2, 0, op4, op5) --Pas de transposition
      1 -> put (op1, op2, 1, op4, op5) --Transposition de +12
      2 -> put (op1, op2, 2, op4, op5) --Transposition de -12
      _ -> put (op1, op2, op3, op4, op5)

    --On modifie le mode miroir
    "3" -> case n of
      0 -> put (op1, op2, op3, 0, op5) --Mode miroir OFF
      1 -> put (op1, op2, op3, 1, op5) --Mode miroir ON
      _ -> put (op1, op2, op3, op4, op5)

    --On modifie la vitesse
    "4" -> if n2 > 0 then put (op1, op2, op3, op4, n2)
      else put (op1, op2, op3, op4, op5)

    --Le parametre à modifier n'existe pas. Les paramètres restent inchangés
    _ -> put (op1, op2, op3, op4, op5)

  --On renvoie les paramètres
  case x of
    --On modifie la valeur du driver de sortie
    "0" -> if n > 0 then return (n, op2, op3, op4, op5) --La valeur du driver de sortie ne peut pas etre négative
      else return (op1, op2, op3, op4, op5)
    
    -- On modifie le numéro de l'instrument choisi
    "1" -> case n of
      1 -> return (op1, 1, op3, op4, op5) 
      2 -> return (op1, 2, op3, op4, op5)
      3 -> return (op1, 3, op3, op4, op5)
      4 -> return (op1, 4, op3, op4, op5)
      5 -> return (op1, 5, op3, op4, op5)
      _ -> return (op1, op2, op3, op4, op5)

    --On modifie le mode de transposition
    "2" -> case n of
      0 -> return (op1, op2, 0, op4, op5) --Pas de transposition
      1 -> return (op1, op2, 1, op4, op5) --Transposition de +12
      2 -> return (op1, op2, 2, op4, op5) --Transposition de -12
      _ -> return (op1, op2, op3, op4, op5)

    --On modifie le mode miroir
    "3" -> case n of
      0 -> return (op1, op2, op3, 0, op5) --Mode miroir OFF
      1 -> return (op1, op2, op3, 1, op5) --Mode miroir ON
      _ -> return (op1, op2, op3, op4, op5)

    --On modifie la vitesse
    "4" -> if n2 > 0 then return (op1, op2, op3, op4, n2)
      else return (op1, op2, op3, op4, op5)

    --Le parametre à modifier n'existe pas. Les paramètres restent inchangés
    _ -> return (op1, op2, op3, op4, op5)


defaultOptions = (0, 1, 0, 0, 1.0) --Options par défaut




--Fonction permettant de créer le Menu
menu :: IO ()
menu = do
  let options = defaultOptions --Options par défaut

  putStrLn "\nMenu Principal - Le Jeu de Mozart\n1 - Jouer un menuet\n2 - Modifier le device de sortie\n3 - Modifier l'instrument\n4 - Modifier le mode de transposition\n5 - Modifier le mode miroir\n6 - Modifier la vitesse\n0 - Fermeture du menu\n\nVeuillez choisir une option :"
  choice <- getLine
  case choice of

    --Choice = 1
    "1" -> do
      putStrLn "Not available" --On joue un menuet
      menu
      return ()

    --Choice = 2
    "2" -> do
      --On print les drivers
      n <- countDevices
      midiDevicePrint (n - 1)

      putStrLn "\nQuel device souhaitez-vous utiliser ?"
      choice2 <- getLine
      --modifyParameters (choice, choice2) State options

      menu
      return ()

    --Choice = 3
    "3" -> do
      menu
      return()

    --Choice = 4
    "4" -> do
      menu
      return ()

    --Choice = 5
    "5" -> do
      menu
      return ()

    --Choice = 6
    "6" -> do
      menu
      return ()

    --Choice = 0
    "0" -> do
      putStrLn "\nFermeture du menu. Au revoir.\n"
      return ()





--Main
main :: IO ()
main = do
  putStrLn "Le Jeu de Mozart"
  menu --On ouvre le menu
  initialize
  n <- countDevices
  midiDevicePrint (n - 1)
  deviceId <- getDefaultOutputDeviceID
  case deviceId of
     Nothing   -> putStrLn "Pas de port Midi par default"
     Just n ->
      do
       result <- openOutput n 1
       case result of
        Left err   -> return ()
        Right stream ->
         do
          play (Note 72 1000 100) stream
          close stream
          return ()
  terminate
  return ()


