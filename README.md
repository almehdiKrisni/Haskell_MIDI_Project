# Haskell_MIDI_Project

Contient l'ensemble du code du projet de Haskell - Le jeu de Mozart.

Dans le cadre du projet il nous a été demandé de réaliser un programme en Haskell qui permet de jouer des menuets composés avec le jeu de Mozart. 

## Dossiers
Pour cela deux dossiers nous ont été fourni : 

1- PortMidi-master : Librairie permettant d'émuler un Port Midi sur ordinateur

2- pcompProject : Projet contenant les fichiers à modifier

## Description des Fichiers modifiés

### 1. MusicLib.hs

- Structure **MusObj** composée de soit : 
    - Une note *(Integer Integer Integer)*
    - Chord *(Integer [MusObj])*
    - Measure *([MusObj] deriving (Show))* 

- **getOnSet** *(MusObj -> Integer)* : Permet de connaitre le nombre de note
- **getDur** *(MusObj -> Integer)* : Afin de calculer la durée d'un objet musical
- **collectMidi** *(MusObj -> Integer -> [(Integer, PMMsg)])* : Permet d'appliquer la fonction collectMidiNote (Midi.hs) à un objet musical
- **play** *(MusObj -> PMStream -> IO())* : Jouer une note
- **countNotes** *(MusObj -> Integer)* : Retourne le nombre de notes d'un objet musical
- **stretch** *(MusObj -> Float -> MusObj)* : Multiplie la durée d'un objet musical par un facteur flottant
- **transpose** *(MusObj -> Integer (n) -> MusObj)* : Additionne au hauteur d'un objet musical n demitons
- **mirror** *(MusObj -> Integer -> MusObj)* : Fait le miroir de toutes les hauteurs d'un objet musical autour d'une hauteur donnée

### 2. Main.hs

- Structure **GameOptions** : 
    - deviceSelect *(DeviceID)* 
    - instrumentNumber *(Integer)*
    - transpositionMode *(Integer)*
    - mirrorMode *(Integer)*
    - speedFactor *(Float)* 

- **midiDevicePrint** *(Int -> IO())* : Affiche les informations à propos d'un device de sortie
- **modifyOutputDevice** *(DeviceID -> State GameOptions DeviceID)* : Modifier le device de sortie
- **modifyInstrumentNumber** *(Integer -> State GameOptions Integer)* : Modifier le numéro de l'instrument choisi
- **modifyTranspositionMode** *(Integer -> State GameOptions Integer)* : Modifier le mode de transposition
- **modifyMirrorMode** *(Integer -> State GameOptions Integer)* : Passer en mode miroir (via une hauteur de 100) 
- **modifyMusicSpeed** *(Float -> State GameOptions Float)* : Changer la vitesse
- **playMeasure** *(GameOptions -> Integer -> IO())* : Fonction permettant de jouer un nombre de menuet passer en paramètre 
- **menu** *(GameOptions -> IO())* : Interface utilisateur (UI)

## Le Menu

En lançant le fichier main.hs un menu s'affiche offrant à l'utilisateur plusieurs choix, selon le nombre rentré dans le terminal.

#### Choix '1' : Jouer un menuet 

Ce choix va permettre de jouer 16 notes choisies aléatoirement parmi une liste de mesures fournit dans le fichier DataBase.hs

#### Choix '2' : Modifier le device de sortie 

Cette option permet de choisir le device de sortie audio

#### Choix '3' : Modifier l'instrument

Permet de choisir l'instrument pour le son
Veuillez vous référer à liste d'instrument sur :
https://soundprogramming.net/file-formats/general-midi-instrument-list/

#### Choix '4' : Modifier le mode de transposition 

Permet d'ajouter ou enlever des demitons à la hauteur. 
- 0 = Sans Transposition
- 1 = +12 demitons
- 2 = -12 demitons

#### Choix '5' : Passer en mode miroir

Passage en mode miroir par rapport à une hauteur de 100.
- 0 = Mode miroir désactivé
- 1 = Mode miroir activé 

#### Choix '6' : Changement de vitesse

Permet de changer la vitesse d'execution via une valeur flottante comprise entre 0.0 et 1000.0. 
Plus la valeur fournie est faible, plus la musique est rapide, et inversement.

#### Choix '7' : Remettre à défaut

Permet de remettre les paramètre modifiés durant l'utilisation du programme à défaut.

#### Choix '0' : Quitter

Quiiter le programme





