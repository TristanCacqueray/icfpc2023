module ProgCon.Syntax where

data Attendee = Attendee
    { attendeeTastes :: [Int]
    , attendeeX :: Float
    , attendeeY :: Float
    }
    deriving (Show, Eq, Ord)

data Problem = Problem
    { problemStageHeight :: Float
    , problemStageWidth :: Float
    , problemMusicians :: [Int]
    , problemRoomHeight :: Float
    , problemRoomWidth :: Float
    , problemAttendees :: [Attendee]
    , problemStageBottomLeft :: (Float, Float)
    }
    deriving (Show, Eq, Ord)

data Solution = Solution
    { solutionPlacements :: [Placement]
    }
    deriving (Show, Eq, Ord)

data Placement = Placement
    { placementX :: Float
    , placementY :: Float
    }
    deriving (Show, Eq, Ord)
