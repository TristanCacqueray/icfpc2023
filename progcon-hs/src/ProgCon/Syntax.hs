module ProgCon.Syntax where

data Attendees = Attendees
    { attendeesTastes :: [Int]
    , attendeesX :: Float
    , attendeesY :: Float
    }
    deriving (Show, Eq, Ord)

data Problem = Problem
    { problemStageHeight :: Float
    , problemStageWidth :: Float
    , problemMusicians :: [Int]
    , problemRoomHeight :: Float
    , problemRoomWidth :: Float
    , problemAttendees :: [Attendees]
    , problemStageBottomLeft :: (Float, Float)
    }
    deriving (Show, Eq, Ord)

data Solution = Solution
    { solutionPlacements :: [Placements]
    }
    deriving (Show, Eq, Ord)

data Placements = Placements
    { placementsX :: Float
    , placementsY :: Float
    }
    deriving (Show, Eq, Ord)
