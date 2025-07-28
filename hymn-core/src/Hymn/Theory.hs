{-# LANGUAGE OverloadedStrings #-}
module Hymn.Theory where

import Hymn.Types

-- | Generate a major scale starting from the given pitch step (includes octave)
generateMajorScale :: PitchStep -> Int -> [Note]
generateMajorScale rootStep octave = 
  let scaleSteps = take 8 $ iterate nextStep rootStep  -- 8 notes: root + 7 scale degrees
      intervals = [0, 2, 4, 5, 7, 9, 11, 12]  -- Major scale intervals including octave
      pitches = zipWith (makePitch rootStep octave) scaleSteps intervals
  in map (\p -> Note (Just p) 4 1 (Just Quarter) (pitchToAccidental p) False False False) pitches

-- Create a pitch with proper accidentals based on the root and interval
makePitch :: PitchStep -> Int -> PitchStep -> Int -> Pitch
makePitch rootStep baseOctave step interval = 
  let rootSemitone = stepToSemitone rootStep
      targetSemitone = (rootSemitone + interval) `mod` 12
      stepSemitone = stepToSemitone step
      -- Calculate the difference, handling wrap-around correctly
      diff = (targetSemitone - stepSemitone + 12) `mod` 12
      alter = case diff of
                0 -> Nothing    -- Natural
                1 -> Just 1     -- Sharp
                2 -> Just 2     -- Double sharp
                11 -> Just (-1) -- Flat
                10 -> Just (-2) -- Double flat
                _ -> Nothing    -- Default to natural for unusual cases
      -- Calculate octave adjustment: use interval to determine octave crossing
      octaveAdjust = if interval >= 12 then 1
                    else if hasWrappedAround rootStep step then 1
                    else 0
  in Pitch step alter (baseOctave + octaveAdjust)

-- Check if we've wrapped around the alphabet (e.g., D -> C)
hasWrappedAround :: PitchStep -> PitchStep -> Bool
hasWrappedAround rootStep currentStep = 
  stepToSemitone currentStep < stepToSemitone rootStep

-- Convert pitch step to semitone (C=0, D=2, E=4, etc.)
stepToSemitone :: PitchStep -> Int
stepToSemitone C = 0
stepToSemitone D = 2
stepToSemitone E = 4
stepToSemitone F = 5
stepToSemitone G = 7
stepToSemitone A = 9
stepToSemitone B = 11

-- Convert pitch alteration to accidental symbol
pitchToAccidental :: Pitch -> Maybe Accidental
pitchToAccidental (Pitch _ alter _) = 
  case alter of
    Just 1 -> Just Sharp
    Just (-1) -> Just Flat
    Just 2 -> Just DoubleSharp
    Just (-2) -> Just DoubleFlat
    _ -> Nothing

-- Helper function to get the next step in the sequence
nextStep :: PitchStep -> PitchStep
nextStep A = B
nextStep B = C
nextStep C = D
nextStep D = E
nextStep E = F
nextStep F = G
nextStep G = A
