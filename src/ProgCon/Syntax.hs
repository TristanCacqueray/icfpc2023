module ProgCon.Syntax where

import Data.Vector.Unboxed qualified as UV

data Attendee = Attendee
    { attendeeTastes :: UV.Vector Int
    , attendeeX :: Int
    , attendeeY :: Int
    }
    deriving (Show, Eq, Ord)

data Problem = Problem
    { problemStageHeight :: Int
    , problemStageWidth :: Int
    , problemMusicians :: UV.Vector Int
    , problemRoomHeight :: Int
    , problemRoomWidth :: Int
    , problemAttendees :: [Attendee]
    , problemStageBottomLeft :: (Int, Int)
    }
    deriving (Show, Eq, Ord)

newtype Solution = Solution
    { solutionPlacements :: UV.Vector (Int, Int)
    }
    deriving (Show, Eq, Ord)
