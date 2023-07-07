module ProgCon.Syntax where

data Attendees = Attendees
    { attendeesTastes :: [Int]
    , attendeesX :: Int
    , attendeesY :: Int
    }
    deriving (Show, Eq, Ord)

data Problem = Problem
    { problemStageHeight :: Int
    , problemStageWidth :: Int
    , problemMusicians :: [Int]
    , problemRoomHeight :: Int
    , problemAttendees :: [Attendees]
    , problemStageBottomLeft :: [Int]
    , problemRoomWidth :: Int
    }
    deriving (Show, Eq, Ord)

data Solution = Solution
    { solutionPlacements :: [Placements]
    }
    deriving (Show, Eq, Ord)

data Placements = Placements
    { placementsX :: Int
    , placementsY :: Int
    }
    deriving (Show, Eq, Ord)
