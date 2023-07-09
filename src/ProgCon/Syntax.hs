{-# LANGUAGE PatternSynonyms #-}

module ProgCon.Syntax where

import Data.Aeson (ToJSON)
import Data.Vector.Mutable qualified as MV
import Data.Vector.Unboxed qualified as UV
import Text.Printf (printf)

newtype ProblemID = ProblemID Int
    deriving newtype (Show, Eq, Ord, Enum, Num, ToJSON)

pattern SpecProblem :: ProblemID
pattern SpecProblem = ProblemID 0

problemBase :: ProblemID -> FilePath
problemBase SpecProblem = "problems/spec"
problemBase (ProblemID pid) = "problems/" <> printf "%02d" pid

problemPath :: ProblemID -> FilePath
problemPath pid = problemBase pid <> "-problem.json"

solutionPath :: ProblemID -> FilePath
solutionPath pid = problemBase pid <> "-solution.json"

data Params = Params
    { seedCount :: Int
    , breedCount :: Int
    , volumeCount :: Int
    , genCount :: Int
    , onlyVolume :: Bool
    }
    deriving (Show)

data ProblemDescription = ProblemDescription
    { name :: ProblemID
    , problem :: Problem
    , -- a dense pillars representation using (x, y, radius)
      pillars :: UV.Vector (Int, Int, Int)
    }

data Attendee = Attendee
    { attendeeTastes :: UV.Vector Int
    , attendeeX :: Int
    , attendeeY :: Int
    }
    deriving (Show, Eq, Ord)

data Pillar = Pillar
    { pillarCenter :: (Int, Int)
    , pillarRadius :: Int
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
    , problemPillars :: [Pillar]
    }

{- | All the positions are stored, that way all the mutation happen in-place.
 in 'toSolution' we keep only the one for the active musician.
-}
newtype GenPlacements = GenPlacements {iov :: MV.IOVector (Int, Int)}

{- | The SolutionDescription contains all the initial placements so that it can be safely re-used in the solver.
 To get the final solution, use "take musicianCount genPlacements"
-}
data SolutionDescription = SolutionDescription
    { score :: Int
    , musicianCount :: Int
    , genPlacements :: GenPlacements
    , genVolumes :: MV.IOVector Float
    }

-- | This is the final solution representation to be submitted
data Solution = Solution
    { solutionPlacements :: UV.Vector (Int, Int)
    , solutionVolumes :: UV.Vector Float
    }
    deriving (Show, Eq, Ord)
