{-# LANGUAGE OverloadedStrings #-}
module Hymn.Theory where

import Hymn.Types

-- | Generate a major scale starting from the given pitch step
generateMajorScale :: PitchStep -> Int -> [Note]
generateMajorScale rootStep octave = 
  let intervals = [0, 2, 4, 5, 7, 9, 11] -- Major scale intervals in semitones
      steps = [rootStep, nextStep rootStep, nextStep (nextStep rootStep), 
               nextStep (nextStep (nextStep rootStep)), 
               nextStep (nextStep (nextStep (nextStep rootStep))),
               nextStep (nextStep (nextStep (nextStep (nextStep rootStep)))),
               nextStep (nextStep (nextStep (nextStep (nextStep (nextStep rootStep)))))]
      pitches = zipWith (\step interval -> 
                  let (oct, alter) = intervalToPitchInfo interval
                  in Pitch step alter (octave + oct)) steps intervals
  in map (\p -> Note (Just p) 4 1 (Just Quarter) Nothing False False False) pitches

-- Helper function to get the next step in the sequence
nextStep :: PitchStep -> PitchStep
nextStep A = B
nextStep B = C
nextStep C = D
nextStep D = E
nextStep E = F
nextStep F = G
nextStep G = A

-- Convert interval to pitch alteration info
intervalToPitchInfo :: Int -> (Int, Maybe Int)
intervalToPitchInfo interval = 
  case interval of
    0 -> (0, Nothing)   -- Root
    2 -> (0, Nothing)   -- Major 2nd
    4 -> (0, Nothing)   -- Major 3rd
    5 -> (0, Nothing)   -- Perfect 4th
    7 -> (0, Nothing)   -- Perfect 5th
    9 -> (0, Nothing)   -- Major 6th
    11 -> (0, Nothing)  -- Major 7th
    _ -> (0, Nothing)   -- Default
