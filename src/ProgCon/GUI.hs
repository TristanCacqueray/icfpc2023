module ProgCon.GUI where

import Graphics.Gloss
import Data.Vector.Unboxed qualified as UV

import ProgCon.Syntax

attendeeSize :: Float
attendeeSize = 3

drawProblem :: Problem -> Solution -> Picture
drawProblem problem solution = Pictures (room : stage : (musicians <> attendees))
  where
    room =
        Color red $
            rectangleWire problem.problemRoomWidth problem.problemRoomHeight
    topX = -1 * problem.problemRoomWidth / 2
    topY = problem.problemRoomHeight / 2

    musicians = map drawMusician (UV.toList solution.solutionPlacements)
    drawMusician (x, y) = Translate (topX + x) (topY - y) $ Circle 10

    attendees = map drawAttendee problem.problemAttendees
    drawAttendee attendee = Translate (topX + attendee.attendeeX) (topY - attendee.attendeeY) $ Circle attendeeSize
    stage =
        Translate
            (topX + problem.problemStageWidth / 2 + stageX)
            (topY - problem.problemStageHeight / 2 - stageY)
            $ Color orange
            $ Polygon
            $ rectanglePath problem.problemStageWidth problem.problemStageHeight
    (stageX, stageY) = problem.problemStageBottomLeft

renderProblem :: Problem -> Solution -> IO ()
renderProblem problem solution = do
    display (InWindow "ICFP Contest 2023" (round wx, round wy) (10, 10)) white (absScale $ drawProblem problem solution)
  where
    absScale = Scale (pscale * 0.98) (pscale * 0.98)
    wx, wy, pscale :: Float
    (wx, wy) = (problem.problemRoomWidth * pscale, problem.problemRoomHeight * pscale)
    pscale = 1 / 5
