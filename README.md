# Haskell_MIDI_Project

Binome :<br/>
KRISNI Almehdi - 3800519<br/>
BADRIOUICHE Mohamed - 3701404

Contient l'ensemble du code du projet de Haskell - Le jeu de Mozart.

Dans le cadre du projet, il nous a été demandé de réaliser un programme en Haskell qui permet de jouer des menuets composés avec le jeu de Mozart. 

## Dossiers
Pour cela, deux dossiers nous ont été fourni : 

1) PortMidi-master : Librairie permettant d'émuler un Port Midi sur ordinateur.

2) pcompProject : Projet contenant les fichiers à modifier.

## Description des Fichiers modifiés

### 1. MusicLib.hs

- Structure **MusObj** représentant soit : 
    - Une note *(Integer Integer Integer)*
    - Chord *(Integer [MusObj])*
    - Measure *([MusObj] deriving (Show))* 

Fonctions présentes dans le fichier :
- **getOnSet** *(MusObj -> Integer)* : Permet de connaitre le nombre de note
- **getDur** *(MusObj -> Integer)* : Afin de calculer la durée d'un objet musical
- **collectMidi** *(MusObj -> Integer -> [(Integer, PMMsg)])* : Permet d'appliquer la fonction collectMidiNote (Midi.hs) à un objet musical
- **play** *(MusObj -> PMStream -> IO())* : Jouer une note
- **countNotes** *(MusObj -> Integer)* : Retourne le nombre de notes d'un objet musical
- **stretch** *(MusObj -> Float -> MusObj)* : Multiplie la durée d'un objet musical par un facteur flottant
- **transpose** *(MusObj -> Integer (n) -> MusObj)* : Additionne à la hauteur d'un objet musical n demitons
- **mirror** *(MusObj -> Integer -> MusObj)* : Fait le miroir de toutes les hauteurs d'un objet musical autour d'une hauteur donnée

### 2. Main.hs

- Structure **GameOptions** contenant : 
    - deviceSelect *(DeviceID)* 
    - instrumentNumber *(Integer)*
    - transpositionMode *(Integer)*
    - mirrorMode *(Integer)*
    - speedFactor *(Float)* 

Fonctions présentes dans le fichier :
- **midiDevicePrint** *(Int -> IO())* : Affiche les informations à propos d'un device de sortie
- **modifyOutputDevice** *(DeviceID -> State GameOptions DeviceID)* : Modifier le device de sortie
- **modifyInstrumentNumber** *(Integer -> State GameOptions Integer)* : Modifier le numéro de l'instrument choisi
- **modifyTranspositionMode** *(Integer -> State GameOptions Integer)* : Modifier le mode de transposition
- **modifyMirrorMode** *(Integer -> State GameOptions Integer)* : Passer en mode miroir (via une hauteur de 100) 
- **modifyMusicSpeed** *(Float -> State GameOptions Float)* : Changer la vitesse
- **playMeasure** *(GameOptions -> Integer -> IO())* : Fonction permettant de jouer un menuet 
Attention - L'appel à playMeasure doit etre réalisé avec l'entier 0 afin de lire exactement 16 mesures.
- **menu** *(GameOptions -> IO())* : Interface utilisateur (UI)

## Menu

Lorsqu'on exécute le Main, un menu apparaît au niveau du terminal. Grâce à cette interface, l'utilisateur peut demander la lecture d'un menuet en fonction des paramètres qu'il aura sélectionnés.

L'utilisateur peut effectuer les actions suivantes : 

#### Choix 1 - Jouer un menuet 

Permet de jouer 16 mesures (menuet) choisies aléatoirement parmi une liste de mesures fournie dans le fichier DataBase.hs.
Le menuet est divisé en deux parties composées chacune de 8 mesures. A chaque partie son ensemble de mesures à jouer.

#### Choix 2 - Modifier le device de sortie 

Permet de choisir le device de sortie audio. Afin d'éviter tout problème, le device de sortie audio par défaut sera utilisé.

#### Choix 3 - Modifier l'instrument

Permet de choisir l'instrument utilisé.
Se référer à liste d'instrument sur :
<br/>https://soundprogramming.net/file-formats/general-midi-instrument-list/

#### Choix 4 - Modifier le mode de transposition 

Permet d'ajouter ou enlever des demitons à la hauteur :
- 0 = Sans Transposition
- 1 = +12 demitons
- 2 = -12 demitons

#### Choix 5 - Passer en mode miroir

Premet le passage en mode miroir par rapport à une hauteur de 60 :
- 0 = Mode miroir désactivé
- 1 = Mode miroir activé 

#### Choix 6 - Changement de vitesse

Permet de changer la vitesse d'exécution via une valeur flottante comprise entre 0.0 et 1000.0. 
<br/>Plus la valeur fournie est faible, plus la musique est rapide, et inversement.

#### Choix 7 - Affichage des options sélectionnées

Permet d'afficher les options en cours d'utilisation.

#### Choix 8 - Réinitialiser les paramètres

Permet de remettre les paramètres modifiés durant l'utilisation du programme à défaut.

#### Choix 0 - Quitter le programme

Permet de quitter le programme.