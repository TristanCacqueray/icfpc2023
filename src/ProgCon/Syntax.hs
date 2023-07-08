module ProgCon.Syntax where

import Data.Vector.Unboxed qualified as UV
import Data.Vector.Mutable qualified as MV
import Data.Aeson (ToJSON)
import Text.Printf (printf)

newtype ProblemID = ProblemID Int
  deriving newtype (Show, ToJSON)

problemBase :: ProblemID -> FilePath
problemBase (ProblemID pid) =  "./problems/" <> printf "%02d" pid

problemPath :: ProblemID -> FilePath
problemPath pid = problemBase pid <> "-problem.json"

solutionPath :: ProblemID -> FilePath
solutionPath pid = problemBase pid <> "-solution.json"

data ProblemDescription = ProblemDescription
    { name :: ProblemID
    , problem :: Problem
    }

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

{- | All the positions are stored, that way all the mutation happen in-place.
 in 'toSolution' we keep only the one for the active musician.
-}
newtype GenPlacements = GenPlacements { iov :: MV.IOVector (Int, Int) }

-- | The SolutionDescription contains all the initial placements so that it can be safely re-used in the solver.
-- To get the final solution, use "take musicianCount genPlacements"
data SolutionDescription = SolutionDescription
  { score :: Int
  , musicianCount :: Int
  , genPlacements :: GenPlacements
  }

-- | This is the final solution representation to be submitted
newtype Solution = Solution
    { solutionPlacements :: UV.Vector (Int, Int)
    }
    deriving (Show, Eq, Ord)
