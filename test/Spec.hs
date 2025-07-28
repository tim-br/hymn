{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Test.QuickCheck
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.List (isInfixOf)
import Hymn.Types
import Hymn.Theory
import Hymn.Render

main :: IO ()
main = hspec $ do
  describe "Music Theory Tests" $ do
    describe "generateMajorScale" $ do
      it "generates C major scale correctly" $ do
        let scale = generateMajorScale C 4
        length scale `shouldBe` 8
        -- Check first and last notes
        pitch (head scale) `shouldBe` Just (Pitch C Nothing 4)
        pitch (last scale) `shouldBe` Just (Pitch C Nothing 5)
      
      it "generates D major scale with correct sharps" $ do
        let scale = generateMajorScale D 4
            pitches = map pitch scale
        -- Should have F# and C#
        pitches !! 2 `shouldBe` Just (Pitch F (Just 1) 4)  -- F#
        pitches !! 6 `shouldBe` Just (Pitch C (Just 1) 5)  -- C#
      
      it "generates F major scale with correct flats" $ do
        let scale = generateMajorScale F 4
            pitches = map pitch scale
        -- Should have Bb
        pitches !! 3 `shouldBe` Just (Pitch B (Just (-1)) 4)  -- Bb

    describe "stepToSemitone" $ do
      it "maps pitch steps correctly" $ do
        stepToSemitone C `shouldBe` 0
        stepToSemitone D `shouldBe` 2
        stepToSemitone E `shouldBe` 4
        stepToSemitone F `shouldBe` 5
        stepToSemitone G `shouldBe` 7
        stepToSemitone A `shouldBe` 9
        stepToSemitone B `shouldBe` 11

  describe "XML Rendering Tests" $ do
    it "renders simple note correctly" $ do
      let note = Note (Just (Pitch C Nothing 4)) 4 1 (Just Quarter) Nothing False False False
          measure = Measure 1 Nothing [MNote note]
          part = Part "P1" [measure]
          score = ScorePartwise "3.1" [ScorePart "P1" "Test"] [part]
          xml = renderScore score
          xmlString = L8.unpack xml
      -- Check that XML contains expected elements using isInfixOf
      xmlString `shouldSatisfy` ("<step>C</step>" `isInfixOf`)
      xmlString `shouldSatisfy` ("<octave>4</octave>" `isInfixOf`)
      xmlString `shouldSatisfy` ("<type>quarter</type>" `isInfixOf`)
