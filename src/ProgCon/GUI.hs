module ProgCon.GUI where

import Data.Vector.Unboxed qualified as UV
import GHC.Float (int2Float)
import Graphics.Gloss hiding (scale)
import Graphics.Gloss.Interface.IO.Animate qualified as GlossIO
import RIO hiding (display)

import Control.Concurrent (forkIO)
import ProgCon.Syntax

newtype ProblemRenderer = ProblemRenderer (IORef (Maybe Picture))

withRenderer :: (ProblemRenderer -> IO ()) -> IO ()
withRenderer cb = do
    pictureRef <- newIORef Nothing
    let makePicture :: IO Picture
        makePicture = do
            mPicture <- readIORef pictureRef
            pure $ fromMaybe (Text "Loading...") mPicture

    GlossIO.animateIO disp bg (const makePicture) \_controller -> do
        cb (ProblemRenderer pictureRef)
  where
    disp = InWindow "ICFP Contest 2023" (winX, winY) (10, 10)
    bg = makeColor 0.6 0.6 0.6 1.0

winX, winY :: Int
(winX, winY) = (1024, 1024)

renderProblem :: MonadIO m => Problem -> Solution -> ProblemRenderer -> m ()
renderProblem problem solution (ProblemRenderer pref) = do
    let scale
            | problem.problemRoomHeight > problem.problemRoomWidth =
                fromIntegral winY / fromIntegral problem.problemRoomHeight
            | otherwise = fromIntegral problem.problemRoomWidth / fromIntegral winX
        pscale = scale * 0.98
    writeIORef pref $ Just $ Scale pscale pscale $ drawProblem problem solution

-- liftIO controller.controllerSetRedraw

attendeeSize :: Float
attendeeSize = 3

drawProblem :: Problem -> Solution -> Picture
drawProblem problem solution = Pictures (room : stage : (pillars <> musicians <> attendees))
  where
    room =
        Color red $
            rectangleWire (int2Float problem.problemRoomWidth) (int2Float problem.problemRoomHeight)
    topX, topY :: Float
    topX = -1 * fromIntegral problem.problemRoomWidth / 2
    topY = fromIntegral problem.problemRoomHeight / 2

    pillars = map drawPillar problem.problemPillars
    drawPillar (Pillar (px, py) radius) = Translate (topX + fromIntegral px) (topY - fromIntegral py) $ Color chartreuse $ Circle (fromIntegral radius)

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
