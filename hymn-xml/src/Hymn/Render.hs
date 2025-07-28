{-# LANGUAGE OverloadedStrings #-}
module Hymn.Render (renderScore, writeMusicXML) where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL
import Text.XML (renderLBS, def)
import Text.XML.Writer (element, elementA, content, document, XML, empty)
import Debug.Trace (trace)
import Hymn.Types

renderScore :: MusicXML -> BL.ByteString
renderScore score = renderLBS def $ document "score-partwise" $ do
  element "part-list" $ mapM_ renderScorePart (partList score)
  mapM_ renderPart (parts score)

writeMusicXML :: FilePath -> MusicXML -> IO ()
writeMusicXML path score = BL.writeFile path (renderScore score)

renderScorePart :: ScorePart -> XML
renderScorePart part = 
  elementA "score-part" [("id", partId part)] $
    element "part-name" $ content (partName part)

renderPart :: Part -> XML
renderPart part = 
  elementA "part" [("id", partIdRef part)] $
    mapM_ renderMeasure (measures part)

renderMeasure :: Measure -> XML
renderMeasure (Measure measureNumber attributes events) = 
    trace (" renderMeasure called with: ") $
  elementA "measure" [("number", T.pack $ show measureNumber)] $ do
    maybe (pure ()) renderAttributes attributes
    mapM_ renderEvent events

renderAttributes :: Attributes -> XML
renderAttributes attrs@(Attributes divisions key time clef) = 
  trace ("renderAttributes called with: " ++ show attrs) $
  element "attributes" $ do
    element "divisions" $ content (T.pack $ show divisions)
    maybe (pure ()) renderKey key
    maybe (pure ()) renderTime time
    maybe (pure ()) renderClef clef

renderKey :: Key -> XML
renderKey (Key fifths mode) = element "key" $ do
  element "fifths" $ content (T.pack $ show fifths)
  maybe (pure ()) (\m -> element "mode" $ content m) mode

renderTime :: TimeSig -> XML
renderTime (TimeSig beats beatType) = element "time" $ do
  element "beats" $ content (T.pack $ show beats)
  element "beat-type" $ content (T.pack $ show beatType)

renderClef :: Clef -> XML
renderClef (Clef sign line) = element "clef" $ do
  element "sign" $ content sign
  element "line" $ content (T.pack $ show line)

renderEvent :: MusicEvent -> XML
renderEvent (MNote note) = renderNote note
renderEvent _ = pure () -- Handle other events as needed

renderNote :: Note -> XML
renderNote (Note pitch dur voice noteType acc rest tieStart tieStop) = element "note" $ do
  case pitch of
    Just (Pitch step alter oct) -> element "pitch" $ do
      element "step" $ content (T.pack $ show step)
      maybe (pure ()) (\a -> element "alter" $ content (T.pack $ show a)) alter
      element "octave" $ content (T.pack $ show oct)
    Nothing -> element "rest" empty
  element "duration" $ content (T.pack $ show dur)
  element "voice" $ content (T.pack $ show voice)
  maybe (pure ()) (\t -> element "type" $ content (T.pack $ show t)) noteType
  maybe (pure ()) (\a -> element "accidental" $ content (T.pack $ show a)) acc