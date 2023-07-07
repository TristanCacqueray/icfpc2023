module ProgCon.Eval where

import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed ((!))

import ProgCon.Syntax

attendeeHappiness :: UV.Vector Int -> Solution -> Attendee -> Float
attendeeHappiness instruments solution attendee = UV.sum $ UV.imap musicianImpact solution.solutionPlacements
  where
    musicianImpact :: Int -> (Float, Float) -> Float
    musicianImpact musician (px, py)
      | isBlocked = 0
      | otherwise = 1_000_000 * taste / (distance ** 2)
     where
       instrument = instruments ! musician
       taste = attendee.attendeeTastes ! instrument
       distance = sqrt ((attendee.attendeeX - px) ** 2 + (attendee.attendeeY - py) ** 2)
       isBlocked = px == py -- TODO

score :: Problem -> Solution -> Float
score problem solution = sum $ map (attendeeHappiness problem.problemMusicians solution) problem.problemAttendees
