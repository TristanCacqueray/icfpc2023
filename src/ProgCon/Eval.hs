module ProgCon.Eval (scoreHappiness) where

import Data.Vector.Unboxed qualified as UV
import Data.Vector.Unboxed ((!))

import ProgCon.Syntax

type MusicianClosenessFactor = UV.Vector Float -- is Float necessary?

attendeeHappiness :: ProblemDescription -> Solution -> MusicianClosenessFactor -> Attendee -> Int
attendeeHappiness problemDesc solution musicianClosenessFactor attendee = UV.sum musiciansHappiness
  where
    musiciansHappiness =  UV.imap musicianImpact solution.solutionPlacements

    musicianImpact :: Int -> (Int, Int) -> Int
    musicianImpact !musician placement
      | isBlocked = 0
      | otherwise =
        let (d,m) = (1_000_000 * taste) `divMod` distance
            baseImpact = d + if m > 0 then 1 else 0
            closenessFactor = musicianClosenessFactor ! musician
        in ceiling $ closenessFactor * fromIntegral baseImpact
     where
       -- the musician's instrument
       instrument = problemDesc.problem.problemMusicians ! musician
       -- the attendee taste for this instrument
       taste = attendee.attendeeTastes ! instrument
       -- the distance between the attendee and the musician
       distance = calcDistance attendee placement
       -- is the musician blocked
       isBlocked = isBlockedPillar || isBlockedMusician

       -- … by a pillar (Extension 1)?
       isBlockedPillar = UV.any checkBlockedPillar problemDesc.pillars
       checkBlockedPillar :: (Int, Int, Int) -> Bool
       checkBlockedPillar (_px, _py, _radius) = False -- TODO

       -- … by another musician?
       isBlockedMusician = UV.any checkBlocked solution.solutionPlacements
       checkBlocked :: (Int, Int) -> Bool
       checkBlocked otherPlacement = otherDistance < distance && isCrossed
        where
          otherDistance = calcDistance attendee otherPlacement
          isCrossed =
            -- See: https://mathworld.wolfram.com/Circle-LineIntersection.html
            let
              radius = 5 :: Int
              (px, py) = otherPlacement
              (x1, y1) = (attendee.attendeeX - px, attendee.attendeeY - py)
              (x2, y2) = (fst placement - px, snd placement - py)
              d = x1 * y2 - x2 * y1
              discriment = radius * radius * otherDistance - d * d
             in discriment > 0

calcDistance :: Attendee -> (Int, Int) -> Int
calcDistance attendee (px, py) = (attendee.attendeeX - px) ^ (2 :: Int) + (attendee.attendeeY - py) ^ (2 :: Int)

-- TODO: add extensions toggle?
scoreHappiness :: ProblemDescription -> Solution -> Int
scoreHappiness problemDesc solution = sum allHappiness
  where
    problem = problemDesc.problem
    allHappiness = map (attendeeHappiness problemDesc solution musicianClosenessFactor) problem.problemAttendees
    musicianClosenessFactor = UV.generate (UV.length problem.problemMusicians) calcClosenessFactor
    -- Extension 2:
    calcClosenessFactor _musician = 1 -- TODO
