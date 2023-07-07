module ProgCon.GUI where

import Graphics.Gloss

import ProgCon.Syntax

attendeeSize :: Float
attendeeSize = 3

drawProblem :: Problem -> Picture
drawProblem problem = Pictures (room : stage : attendees)
  where
    room =
        Color red $
            rectangleWire problem.problemRoomWidth problem.problemRoomHeight
    topX = -1 * problem.problemRoomWidth / 2
    topY = problem.problemRoomHeight / 2
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

renderProblem :: Problem -> IO ()
renderProblem problem = do
    display (InWindow "ICFP Contest 2023" (round wx, round wy) (10, 10)) white (absScale $ drawProblem problem)
  where
    absScale = Scale (pscale * 0.98) (pscale * 0.98)
    wx, wy, pscale :: Float
    (wx, wy) = (problem.problemRoomWidth * pscale, problem.problemRoomHeight * pscale)
    pscale = 1 / 5
