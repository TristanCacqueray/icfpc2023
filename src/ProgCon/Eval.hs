module ProgCon.Eval (score) where

import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed ((!))

import ProgCon.Syntax

attendeeHappiness :: UV.Vector Int -> Solution -> Attendee -> Float
attendeeHappiness instruments solution attendee = UV.sum $ UV.imap musicianImpact solution.solutionPlacements
  where
    musicianImpact :: Int -> (Float, Float) -> Float
    musicianImpact musician placement
      | isBlocked = 0
      | otherwise = 1_000_000 * taste / (distance ** 2)
     where
       -- the musician's instrument
       instrument = instruments ! musician
       -- the attendee taste for this instrument
       taste = attendee.attendeeTastes ! instrument
       -- the distance between the attendee and the musician
       distance = calcDistance attendee placement
       -- is the musician blocked by another musician?
       isBlocked = UV.any checkBlocked solution.solutionPlacements
       checkBlocked :: (Float, Float) -> Bool
       checkBlocked otherPlacement = otherDistance < distance && isCrossed
        where
          otherDistance = calcDistance attendee otherPlacement
          isCrossed =
            -- See: https://mathworld.wolfram.com/Circle-LineIntersection.html
            let
              radius = 5
              d = calcD attendee otherPlacement
              discriment = radius ** 2 * otherDistance ** 2 - d ** 2
             in discriment >= 0

calcD :: Attendee -> (Float, Float) -> Float
calcD attendee (x2, y2) = x1 * y2 - x2 * y1
  where
    (x1, y1) = (attendee.attendeeX, attendee.attendeeY)

calcDistance :: Attendee -> (Float, Float) -> Float
calcDistance attendee (px, py) = sqrt ((attendee.attendeeX - px) ** 2 + (attendee.attendeeY - py) ** 2)

score :: Problem -> Solution -> Float
score problem solution = sum $ map (attendeeHappiness problem.problemMusicians solution) problem.problemAttendees
