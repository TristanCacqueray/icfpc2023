module ProgCon.Eval (scoreHappiness) where

import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed ((!))

import ProgCon.Syntax

attendeeHappiness :: UV.Vector Int -> Solution -> Attendee -> Int
attendeeHappiness instruments solution attendee = UV.sum $ UV.imap musicianImpact solution.solutionPlacements
  where
    musicianImpact :: Int -> (Float, Float) -> Int
    musicianImpact !musician placement
      | isBlocked = 0
      | otherwise = ceiling $! (1_000_000 * taste) / distance
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
              (px, py) = otherPlacement
              (x1, y1) = (attendee.attendeeX - px, attendee.attendeeY - py)
              (x2, y2) = (fst placement - px, snd placement - py)
              d = x1 * y2 - x2 * y1
              discriment = radius ** 2 * otherDistance - d ** 2
             in discriment >= 0

calcDistance :: Attendee -> (Float, Float) -> Float
calcDistance attendee (px, py) = (attendee.attendeeX - px) ** 2 + (attendee.attendeeY - py) ** 2

scoreHappiness :: Problem -> Solution -> Int
scoreHappiness problem solution = sum $ map (attendeeHappiness problem.problemMusicians solution) problem.problemAttendees
