module ProgCon.GUI where

import Data.Vector.Unboxed qualified as UV
import GHC.Float (int2Float)
import Graphics.Gloss hiding (scale)
import Graphics.Gloss.Interface.IO.Display qualified as GlossIO
import RIO hiding (display)

import ProgCon.Syntax

data ProblemRenderer = ProblemRenderer
    { pictureRef :: IORef (Maybe Picture)
    , controller :: GlossIO.Controller
    }

withRenderer :: (ProblemRenderer -> IO ()) -> IO ()
withRenderer cb = do
    pictureRef <- newIORef Nothing
    let makePicture :: IO Picture
        makePicture = do
            mPicture <- readIORef pictureRef
            pure $ fromMaybe (Text "Loading...") mPicture

    backendVar <- newEmptyMVar

    let runGloss :: IO ()
        runGloss = GlossIO.displayIO disp bg makePicture (putMVar backendVar)

    let runSolver :: IO ()
        runSolver = do
            backend <- takeMVar backendVar
            cb (ProblemRenderer pictureRef backend)

    withAsync runGloss \_ -> do
        runSolver
  where
    disp = InWindow "ICFP Contest 2023" (winX, winY) (10, 10)
    bg = makeColor 0.6 0.6 0.6 1.0

winX, winY :: Int
(winX, winY) = (1024, 1024)

renderProblem :: MonadIO m => Problem -> Solution -> ProblemRenderer -> m ()
renderProblem problem solution renderer = do
    let scale
            | problem.problemRoomHeight > problem.problemRoomWidth =
                fromIntegral winY / fromIntegral problem.problemRoomHeight
            | otherwise = fromIntegral problem.problemRoomWidth / fromIntegral winX
        pscale = scale * 0.98
    writeIORef renderer.pictureRef $ Just $ Scale pscale pscale $ drawProblem problem solution
    liftIO renderer.controller.controllerSetRedraw

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
