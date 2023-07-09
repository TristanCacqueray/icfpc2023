module ProgCon.Eval (scoreHappiness, showScore) where

import Data.Vector.Unboxed ((!))
import Data.Vector.Unboxed qualified as UV
import Fmt

import ProgCon.Syntax

type MusicianClosenessFactor = UV.Vector Float -- is Float necessary?

attendeeHappiness :: ProblemDescription -> Solution -> MusicianClosenessFactor -> Attendee -> Int
attendeeHappiness problemDesc solution musicianClosenessFactor attendee = UV.sum musiciansHappiness
  where
    musiciansHappiness = UV.imap musicianImpact solution.solutionPlacements

    musicianImpact :: Int -> (Int, Int) -> Int
    musicianImpact !musician placement
        | isBlocked = 0
        | otherwise =
            let baseI :: Float -- The I(k) definition, without the round up
                baseI = fromIntegral (1_000_000 * taste) / fromIntegral distance
                baseImpact :: Int -- round up according to I definition
                baseImpact = ceiling baseI
                closenessFactor = musicianClosenessFactor ! musician
                volume = solution.solutionVolumes ! musician
                -- round up again according to the new score definition
             in ceiling $ volume * closenessFactor * fromIntegral baseImpact
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
        checkBlockedPillar (px, py, radius) = isCrossed
          where
            otherPlacement = (px, py)
            otherDistance = calcDistance attendee otherPlacement
            isCrossed = lineCrossCircle attendee placement otherDistance radius otherPlacement

        -- … by another musician?
        isBlockedMusician = UV.any checkBlocked solution.solutionPlacements
        checkBlocked :: (Int, Int) -> Bool
        checkBlocked otherPlacement = otherDistance < distance && isCrossed
          where
            otherDistance = calcDistance attendee otherPlacement
            isCrossed = lineCrossCircle attendee placement otherDistance 5 otherPlacement

{- | Check if the line between two points is blocked by a third point of a given radius (exclusive).
 See: https://mathworld.wolfram.com/Circle-LineIntersection.html
-}
lineCrossCircle :: Attendee -> (Int, Int) -> Int -> Int -> (Int, Int) -> Bool
lineCrossCircle attendee (mx, my) distance radius (px, py) = discriment > 0
  where
    (x1, y1) = (attendee.attendeeX - px, attendee.attendeeY - py)
    (x2, y2) = (mx - px, my - py)
    d = x1 * y2 - x2 * y1
    discriment = radius * radius * distance - d * d

-- | This return the distance squared
calcDistance :: Attendee -> (Int, Int) -> Int
calcDistance attendee (px, py) = (attendee.attendeeX - px) ^ (2 :: Int) + (attendee.attendeeY - py) ^ (2 :: Int)

-- | This return the regular distance
calcDistanceMusician :: (Int, Int) -> (Int, Int) -> Float
calcDistanceMusician (x, y) (px, py) =
    let dx = fromIntegral (x - px)
        dy = fromIntegral (y - py)
     in sqrt (dx ** 2 + dy ** 2)

-- TODO: add extensions toggle?
scoreHappiness :: ProblemDescription -> Solution -> Int
scoreHappiness problemDesc solution = sum allHappiness
  where
    problem = problemDesc.problem
    allHappiness = map (attendeeHappiness problemDesc solution musicianClosenessFactor) problem.problemAttendees
    musicianCount = UV.length problem.problemMusicians
    musicianClosenessFactor = UV.generate musicianCount calcClosenessFactor
    -- Extension 2: we pre-compute all the factor in advance
    calcClosenessFactor musician
        | problemDesc.name > 0 && problemDesc.name < 56 = 1 -- extension is disabled for the first problems
        | otherwise = 1 + UV.sum (UV.generate musicianCount calcMusicianDistance)
      where
        instrument = problemDesc.problem.problemMusicians UV.! musician
        musicianPos = solution.solutionPlacements UV.! musician
        calcMusicianDistance otherMusician
            | otherMusician == musician || instrument /= otherInstrument = 0
            | otherwise =
                let d = calcDistanceMusician musicianPos (solution.solutionPlacements UV.! otherMusician)
                 in 1 / d
          where
            otherInstrument = problemDesc.problem.problemMusicians UV.! otherMusician

showScore :: Int -> String
showScore s = "" +| commaizeF s |+ ""
