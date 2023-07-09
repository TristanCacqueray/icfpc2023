{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generated with http://json-to-haskell.chrispenner.ca/
module ProgCon.Parser (loadProblemPath, loadSolutionPath, saveSolutionPath) where

import ProgCon.Syntax

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed qualified as VU
import Say (sayString)

loadJSON :: FromJSON a => FilePath -> IO a
loadJSON fp =
    Aeson.eitherDecodeFileStrict fp >>= \case
        Right m -> pure m
        Left e -> error $ fp <> ": aeson error: " <> e

loadProblemPath :: ProblemID -> FilePath -> IO ProblemDescription
loadProblemPath pid fp = do
    problem <- loadJSON @Problem fp
    let pillars = UV.fromList (map toDensePillar problem.problemPillars)
    pure $ ProblemDescription pid problem pillars
  where
    toDensePillar :: Pillar -> (Int, Int, Int)
    toDensePillar (Pillar (px, py) radius) = (px, py, radius)

-- | loadSolutionPath deserialize json array into MV.IOVector
loadSolutionPath :: FilePath -> IO SolutionDescription
loadSolutionPath fp = do
    -- NOTE: Keep this tuple in sync with the 'saveSolutionPath'
    (score, musicianCount, placements, volumes) <- loadJSON fp
    iov <- V.thaw placements
    genVolumes <- V.thaw volumes
    let genPlacements = GenPlacements iov
    pure $ SolutionDescription{score, musicianCount, genPlacements, genVolumes}

-- | saveSolutionPath serialize MV.IOVector into json array
saveSolutionPath :: SolutionDescription -> FilePath -> IO ()
saveSolutionPath solutionDesc fp = do
    arr <- V.freeze solutionDesc.genPlacements.iov
    vol <- V.freeze solutionDesc.genVolumes
    -- NOTE: Keep this tuple in sync with the 'loadSolutionPath'
    let tup = (solutionDesc.score, solutionDesc.musicianCount, arr, vol)
    Aeson.encodeFile fp tup
    sayString $ "saved " <> fp

instance ToJSON Solution where
    toJSON Solution{..} =
        object
            [ "placements" .= map toObj (VU.toList solutionPlacements)
            , "volumes" .= solutionVolumes
            ]
      where
        toObj (x, y) = object ["x" .= x, "y" .= y]

instance FromJSON Solution where
    parseJSON (Object v) = do
        arr <- v .: "placements"
        solutionVolumes <- v .: "volumes"
        solutionPlacements <- VU.fromList <$> traverse fromObj arr
        pure $ Solution{..}
      where
        fromObj (Object obj) = (,) <$> obj .: "x" <*> obj .: "y"
        fromObj invalid = do
            prependFailure
                "parsing Solution placement failed, "
                (typeMismatch "Object" invalid)
    parseJSON invalid = do
        prependFailure
            "parsing Solution failed, "
            (typeMismatch "Object" invalid)

instance FromJSON Attendee where
    parseJSON (Object v) = do
        attendeeTastes <- v .: "tastes"
        attendeeX <- v .: "x"
        attendeeY <- v .: "y"
        pure $ Attendee{..}
    parseJSON invalid = do
        prependFailure
            "parsing Attendee failed, "
            (typeMismatch "Object" invalid)

instance FromJSON Pillar where
    parseJSON (Object v) = do
        pillarRadius <- v .: "radius"
        pillarCenter <- v .: "center"
        pure $ Pillar{..}
    parseJSON invalid = do
        prependFailure
            "parsing Pillar failed, "
            (typeMismatch "Object" invalid)

instance FromJSON Problem where
    parseJSON (Object v) = do
        problemStageHeight <- v .: "stage_height"
        problemStageWidth <- v .: "stage_width"
        problemMusicians <- v .: "musicians"
        problemRoomHeight <- v .: "room_height"
        problemAttendees <- v .: "attendees"
        problemStageBottomLeft <- v .: "stage_bottom_left"
        problemRoomWidth <- v .: "room_width"
        problemPillars <- v .: "pillars"
        pure $ Problem{..}
    parseJSON invalid = do
        prependFailure
            "parsing Problem failed, "
            (typeMismatch "Object" invalid)
