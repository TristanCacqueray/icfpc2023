{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Generated with http://json-to-haskell.chrispenner.ca/
module ProgCon.Parser (loadJSON, writeSolution) where

import ProgCon.Syntax

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.ByteString.Lazy qualified as BSL
import Data.Vector.Unboxed qualified as VU

loadJSON :: FromJSON a => FilePath -> IO a
loadJSON fp =
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
            [ "placements" .= map toObj (VU.toList solutionPlacements)
            ]
      where
        toObj (x, y) = object ["x" .= x, "y" .= y]

instance FromJSON Solution where
    parseJSON (Object v) = do
        arr <- v .: "placements"
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

instance ToJSON Attendee where
    toJSON Attendee{..} =
        object
            [ "tastes" .= attendeeTastes
            , "x" .= attendeeX
            , "y" .= attendeeY
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
