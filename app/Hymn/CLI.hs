module Hymn.CLI where

import Options.Applicative
import Hymn.Render (writeMusicXML)
import Hymn.Types

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
  let exampleNote = Note (Just (Pitch C Nothing 4)) 1 1 (Just Quarter) Nothing False False False
      exampleMeasure = Measure 1 Nothing [MNote exampleNote]
      examplePart = Part "P1" [exampleMeasure]
      exampleScore = ScorePartwise "3.1" [ScorePart "P1" "Example Part"] [examplePart]
  writeMusicXML "output.musicxml" exampleScore
  putStrLn "Wrote output.musicxml"