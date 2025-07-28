{-# LANGUAGE OverloadedStrings #-}
module Hymn.Types where

import Data.Text (Text)

-- Root Document
data MusicXML = ScorePartwise
  { version :: Text
  , partList :: [ScorePart]
  , parts :: [Part]
  } deriving (Show)

-- Part Metadata
data ScorePart = ScorePart
  { partId :: Text
  , partName :: Text
  } deriving (Show)

-- Musical Content
data Part = Part
  { partIdRef :: Text
  , measures :: [Measure]
  } deriving (Show)

data Measure = Measure
  { measureNumber :: Int
  , attributes :: Maybe Attributes
  , events :: [MusicEvent]
  } deriving (Show)

-- Events inside a Measure
data MusicEvent
  = MNote Note
  | MDirection Direction
  | MBackup Int
  | MForward Int
  deriving (Show)

-- Note Element
data Note = Note
  { pitch :: Maybe Pitch
  , duration :: Int
  , voice :: Int
  , noteType :: Maybe NoteType
  , accidental :: Maybe Accidental
  , rest :: Bool
  , tieStart :: Bool
  , tieStop :: Bool
  } deriving (Show)

-- Pitch Specification
data Pitch = Pitch
  { step :: PitchStep
  , alter :: Maybe Int
  , octave :: Int
  } deriving (Show)

data PitchStep = A | B | C | D | E | F | G
  deriving (Show)

-- Note Types (duration names) - removed deriving (Show)
data NoteType = Whole | Half | Quarter | Eighth | Sixteenth | ThirtySecond

-- Custom Show instance for MusicXML output
instance Show NoteType where
  show Whole = "whole"
  show Half = "half" 
  show Quarter = "quarter"
  show Eighth = "eighth"
  show Sixteenth = "16th"
  show ThirtySecond = "32nd"

-- Accidental Markings
data Accidental = DoubleFlat | Flat | Natural | Sharp | DoubleSharp
  deriving (Show)

-- Direction (text, dynamics, etc.)
data Direction = Direction
  { directionType :: Text  -- Simplified
  , placement :: Maybe Text
  } deriving (Show)

-- Attributes for a measure
data Attributes = Attributes
  { divisions :: Int
  , key :: Maybe Key
  , time :: Maybe TimeSig
  , clef :: Maybe Clef
  } deriving (Show)

data Key = Key
  { fifths :: Int
  , mode :: Maybe Text
  } deriving (Show)

data TimeSig = TimeSig
  { beats :: Int
  , beatType :: Int
  } deriving (Show)

data Clef = Clef
  { sign :: Text
  , line :: Int
  } deriving (Show)
