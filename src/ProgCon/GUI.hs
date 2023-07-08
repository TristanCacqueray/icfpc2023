module ProgCon.GUI where

import Graphics.Gloss
import Data.Vector.Unboxed qualified as UV
import GHC.Float (int2Float)

import ProgCon.Syntax

attendeeSize :: Float
attendeeSize = 3

drawProblem :: Problem -> Solution -> Picture
drawProblem problem solution = Pictures (room : stage : (musicians <> attendees))
  where
    room =
        Color red $
            rectangleWire (int2Float problem.problemRoomWidth) (int2Float problem.problemRoomHeight)
    topX, topY :: Float
    topX = -1 * fromIntegral problem.problemRoomWidth / 2
    topY = fromIntegral problem.problemRoomHeight / 2

    musicians = map drawMusician (UV.toList solution.solutionPlacements)
    drawMusician (x, y) = Translate (topX + fromIntegral x) (topY - fromIntegral y) $ Circle 10

    attendees = map drawAttendee problem.problemAttendees
    drawAttendee attendee = Translate (topX + fromIntegral attendee.attendeeX) (topY - fromIntegral attendee.attendeeY) $ Circle attendeeSize
    stage =
        Translate
            (topX + int2Float problem.problemStageWidth / 2 + int2Float stageX)
            (topY - int2Float problem.problemStageHeight / 2 - int2Float stageY)
            $ Color orange
            $ Polygon
            $ rectanglePath (int2Float problem.problemStageWidth) (int2Float problem.problemStageHeight)
    (stageX, stageY) = problem.problemStageBottomLeft

renderProblem :: Problem -> Solution -> IO ()
renderProblem problem solution = do
    display (InWindow "ICFP Contest 2023" (round wx, round wy) (10, 10)) white (absScale $ drawProblem problem solution)
  where
    absScale = Scale (pscale * 0.98) (pscale * 0.98)
    wx, wy, pscale :: Float
    (wx, wy) = (fromIntegral problem.problemRoomWidth * pscale, fromIntegral problem.problemRoomHeight * pscale)
    pscale = 1 / 5
