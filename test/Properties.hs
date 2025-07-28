{-# LANGUAGE OverloadedStrings #-}
module Properties where

import Test.QuickCheck
import Hymn.Types
import Hymn.Theory

-- QuickCheck instances
instance Arbitrary PitchStep where
  arbitrary = elements [C, D, E, F, G, A, B]

instance Arbitrary NoteType where
  arbitrary = elements [Whole, Half, Quarter, Eighth, Sixteenth, ThirtySecond]

-- Properties to test
prop_scaleHasEightNotes :: PitchStep -> Int -> Bool
prop_scaleHasEightNotes step octave = 
  length (generateMajorScale step octave) == 8

prop_scaleStartsAndEndsWithSameStep :: PitchStep -> Int -> Bool
prop_scaleStartsAndEndsWithSameStep step octave = 
  let scale = generateMajorScale step octave
      firstStep = case pitch (head scale) of
                   Just (Pitch s _ _) -> s
                   Nothing -> error "No pitch"
      lastStep = case pitch (last scale) of
                  Just (Pitch s _ _) -> s
                  Nothing -> error "No pitch"
  in firstStep == lastStep

prop_octaveIncreases :: PitchStep -> Int -> Bool
prop_octaveIncreases step baseOctave = 
  let scale = generateMajorScale step baseOctave
      firstOctave = case pitch (head scale) of
                     Just (Pitch _ _ o) -> o
                     Nothing -> error "No pitch"
      lastOctave = case pitch (last scale) of
                    Just (Pitch _ _ o) -> o
                    Nothing -> error "No pitch"
  in lastOctave >= firstOctave

runProperties :: IO ()
runProperties = do
  quickCheck prop_scaleHasEightNotes
  quickCheck prop_scaleStartsAndEndsWithSameStep
  quickCheck prop_octaveIncreases
