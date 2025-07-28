{-# LANGUAGE OverloadedStrings #-}
module Hymn.CLI where

import Options.Applicative
import Hymn.Render (writeMusicXML)
import Hymn.Types
import Hymn.Theory (generateMajorScale)

runCLI :: IO ()
runCLI = execParser opts >>= handleCommand
  where
    opts = info (commands <**> helper)
      ( fullDesc
     <> progDesc "hymn - a MusicXML generator"
     <> header "hymn - a sacred Haskell music tool" )

data Command = Version | Generate

commands :: Parser Command
commands = subparser
  ( command "version" (info (pure Version) (progDesc "Show version"))
 <> command "generate" (info (pure Generate) (progDesc "Generate MusicXML"))
  )

handleCommand :: Command -> IO ()
handleCommand Version  = putStrLn "hymn v0.1.0"
handleCommand Generate = do
  let attributes = Attributes 4 (Just (Key 0 Nothing)) (Just (TimeSig 4 4)) (Just (Clef "G" 2))
      scaleNotes = generateMajorScale D 4
      noteEvents = map MNote scaleNotes
      exampleMeasure = Measure 1 (Just attributes) noteEvents
      examplePart = Part "P1" [exampleMeasure]
      exampleScore = ScorePartwise "3.1" [ScorePart "P1" "C Major Scale"] [examplePart]
  writeMusicXML "scale.musicxml" exampleScore
  putStrLn "Wrote C Major scale to scale.musicxml"