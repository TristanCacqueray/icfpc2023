module ProgCon.Syntax where

import Data.Vector.Unboxed qualified as UV

data Attendee = Attendee
    { attendeeTastes :: UV.Vector Float
    , attendeeX :: Float
    , attendeeY :: Float
    }
    deriving (Show, Eq, Ord)

data Problem = Problem
    { problemStageHeight :: Float
    , problemStageWidth :: Float
    , problemMusicians :: UV.Vector Int
    , problemRoomHeight :: Float
    , problemRoomWidth :: Float
    , problemAttendees :: [Attendee]
    , problemStageBottomLeft :: (Float, Float)
    }
    deriving (Show, Eq, Ord)

newtype Solution = Solution
    { solutionPlacements :: UV.Vector (Float, Float)
    }
    deriving (Show, Eq, Ord)
