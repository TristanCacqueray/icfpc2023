{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generated with http://json-to-haskell.chrispenner.ca/
module ProgCon.Parser (loadProblem, writeSolution) where

import ProgCon.Syntax

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.ByteString.Lazy qualified as BSL

loadProblem :: FilePath -> IO Problem
loadProblem fp =
    Aeson.eitherDecodeFileStrict fp >>= \case
        Right m -> pure m
        Left e -> error $ fp <> ": aeson error: " <> e

writeSolution :: Solution -> IO ()
writeSolution solution = do
    BSL.putStr (Aeson.encode solution)
    BSL.putStr "\n"

instance ToJSON Solution where
    toJSON Solution{..} =
        object
            [ "placements" .= solutionPlacements
            ]

instance ToJSON Placements where
    toJSON Placements{..} =
        object
            [ "x" .= placementsX
            , "y" .= placementsY
            ]

instance FromJSON Solution where
    parseJSON (Object v) = do
        solutionPlacements <- v .: "placements"
        pure $ Solution{..}
    parseJSON invalid = do
        prependFailure
            "parsing Solution failed, "
            (typeMismatch "Object" invalid)

instance FromJSON Placements where
    parseJSON (Object v) = do
        placementsX <- v .: "x"
        placementsY <- v .: "y"
        pure $ Placements{..}
    parseJSON invalid = do
        prependFailure
            "parsing Placements failed, "
            (typeMismatch "Object" invalid)

instance ToJSON Attendees where
    toJSON Attendees{..} =
        object
            [ "tastes" .= attendeesTastes
            , "x" .= attendeesX
            , "y" .= attendeesY
            ]

instance ToJSON Problem where
    toJSON Problem{..} =
        object
            [ "stage_height" .= problemStageHeight
            , "stage_width" .= problemStageWidth
            , "musicians" .= problemMusicians
            , "room_height" .= problemRoomHeight
            , "attendees" .= problemAttendees
            , "stage_bottom_left" .= problemStageBottomLeft
            , "room_width" .= problemRoomWidth
            ]

instance FromJSON Attendees where
    parseJSON (Object v) = do
        attendeesTastes <- v .: "tastes"
        attendeesX <- v .: "x"
        attendeesY <- v .: "y"
        pure $ Attendees{..}
    parseJSON invalid = do
        prependFailure
            "parsing Attendees failed, "
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
        pure $ Problem{..}
    parseJSON invalid = do
        prependFailure
            "parsing Problem failed, "
            (typeMismatch "Object" invalid)
