module ProgCon.Eval (scoreHappiness) where

import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed ((!))

import ProgCon.Syntax

attendeeHappiness :: UV.Vector Int -> UV.Vector (Int, Float, Float) -> Attendee -> Int
attendeeHappiness instruments solution attendee = UV.sum $ UV.map musicianImpact solution
  where
    musicianImpact :: (Int, Float, Float) -> Int
    musicianImpact (musician, px, py)
      | isBlocked = 0
      | otherwise = ceiling $! (1_000_000 * taste) / distance
     where
       -- the musician's instrument
       instrument = instruments ! musician
       -- the attendee taste for this instrument
       taste = attendee.attendeeTastes ! instrument
       -- the distance between the attendee and the musician
       distance = calcDistance attendee (px, py)
       -- is the musician blocked by another musician?
       isBlocked = UV.any checkBlocked solution
       checkBlocked :: (Int, Float, Float) -> Bool
       checkBlocked (otherMusician, ox, oy) = otherInstrument /= instrument && otherDistance < distance && isCrossed
        where
          otherDistance = calcDistance attendee (ox, oy)
          otherInstrument = instruments ! otherMusician
          isCrossed =
            -- See: https://mathworld.wolfram.com/Circle-LineIntersection.html
            let
              radius = 5
              (x1, y1) = (attendee.attendeeX - px, attendee.attendeeY - py)
              (x2, y2) = (px - ox, py - oy)
              d = x1 * y2 - x2 * y1
              discriment = radius ** 2 * otherDistance - d ** 2
             in discriment >= 0

calcDistance :: Attendee -> (Float, Float) -> Float
calcDistance attendee (px, py) = (attendee.attendeeX - px) ** 2 + (attendee.attendeeY - py) ** 2

scoreHappiness :: Problem -> Solution -> Int
scoreHappiness problem solution = sum $ map (attendeeHappiness problem.problemMusicians indexedPlacements) problem.problemAttendees
  where
    indexedPlacements = UV.imap (\idx (x, y) -> (idx, x, y)) solution.solutionPlacements
