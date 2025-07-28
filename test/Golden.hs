{-# LANGUAGE OverloadedStrings #-}
module Golden where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import System.FilePath
import Hymn.Types
import Hymn.Theory
import Hymn.Render

-- Generate expected XML output for comparison
generateGoldenFiles :: IO ()
generateGoldenFiles = do
  -- C Major scale (no accidentals)
  let cMajor = generateTestScore C "C Major Scale"
  BL.writeFile "test/golden/c-major.xml" (renderScore cMajor)
  
  -- D Major scale (2 sharps)
  let dMajor = generateTestScore D "D Major Scale"
  BL.writeFile "test/golden/d-major.xml" (renderScore dMajor)
  
  -- F Major scale (1 flat)
  let fMajor = generateTestScore F "F Major Scale"
  BL.writeFile "test/golden/f-major.xml" (renderScore fMajor)

generateTestScore :: PitchStep -> String -> MusicXML
generateTestScore step name = 
  let scale = generateMajorScale step 4
      attributes = Attributes 4 (Just (Key 0 Nothing)) (Just (TimeSig 4 4)) (Just (Clef "G" 2))
      measure = Measure 1 (Just attributes) (map MNote scale)
      part = Part "P1" [measure]
  in ScorePartwise "3.1" [ScorePart "P1" (T.pack name)] [part]

-- Test that current output matches golden files
testGolden :: IO ()
testGolden = do
  testGoldenFile C "c-major.xml"
  testGoldenFile D "d-major.xml"
  testGoldenFile F "f-major.xml"

testGoldenFile :: PitchStep -> FilePath -> IO ()
testGoldenFile step filename = do
  let score = generateTestScore step (show step ++ " Major Scale")
      actual = renderScore score
  expected <- BL.readFile ("test/golden/" ++ filename)
  if actual == expected
    then putStrLn $ "✓ " ++ filename ++ " matches golden file"
    else putStrLn $ "✗ " ++ filename ++ " differs from golden file"
