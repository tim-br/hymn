{-# LANGUAGE OverloadedStrings #-}
module Integration where

import System.IO.Temp
import System.Directory
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Hymn.Types
import Hymn.Theory
import Hymn.Render

-- Test that we can generate and write valid XML files
testAllScales :: IO ()
testAllScales = do
  putStrLn "Testing all major scales..."
  mapM_ testScale [C, D, E, F, G, A, B]
  putStrLn "All scales generated successfully!"

testScale :: PitchStep -> IO ()
testScale step = withSystemTempDirectory "hymn-test" $ \tmpDir -> do
  let filename = tmpDir ++ "/" ++ show step ++ "-major.xml"
      scale = generateMajorScale step 4
      attributes = Attributes 4 (Just (Key 0 Nothing)) (Just (TimeSig 4 4)) (Just (Clef "G" 2))
      measure = Measure 1 (Just attributes) (map MNote scale)
      part = Part "P1" [measure]
      score = ScorePartwise "3.1" [ScorePart "P1" (T.pack (show step ++ " Major Scale"))] [part]
  
  writeMusicXML filename score
  exists <- doesFileExist filename
  if exists
    then putStrLn $ "✓ " ++ show step ++ " major scale generated"
    else error $ "✗ Failed to generate " ++ show step ++ " major scale"
