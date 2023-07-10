module ProgCon.GUI where

import Data.Vector qualified as V
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
    writeIORef renderer.pictureRef $ Just $ windowScale problem $ drawProblem problem solution
    liftIO renderer.controller.controllerSetRedraw

attendeeSize :: Float
attendeeSize = 3

directRender :: Problem -> Solution -> IO ()
directRender problem solution = display disp bg picture
  where
    disp = InWindow "ICFP Contest 2023" winDim (10, 10)
    winDim = (1024, 1024)
    bg = greyN 0.6
    picture = windowScale problem $ drawProblem problem solution

windowScale :: Problem -> Picture -> Picture
windowScale problem = Scale pscale pscale
  where
    scale
        | problem.problemRoomHeight > problem.problemRoomWidth =
            fromIntegral winY / fromIntegral problem.problemRoomHeight
        | otherwise = fromIntegral winX / fromIntegral problem.problemRoomWidth
    pscale = scale * 0.98

drawProblem :: Problem -> Solution -> Picture
drawProblem problem solution = Pictures (room : stage : (pillars <> musicians <> attendees))
  where
    room :: Picture
    room =
        Color red $
            rectangleWire
                (int2Float problem.problemRoomWidth)
                (int2Float problem.problemRoomHeight)

    toAbs :: Float -> Float -> Picture -> Picture
    toAbs x y = Translate (topX + x) (topY - y)
      where
        topX, topY :: Float
        topX = -1 * fromIntegral problem.problemRoomWidth / 2
        topY = fromIntegral problem.problemRoomHeight / 2

    stage :: Picture
    stage =
        toAbs
            (stageWidth / 2 + int2Float stageX)
            (stageHeight / 2 + int2Float stageY)
            $ Color orange
            $ Polygon
            $ rectanglePath stageWidth stageHeight
      where
        stageWidth = int2Float problem.problemStageWidth
        stageHeight = int2Float problem.problemStageHeight
        (stageX, stageY) = problem.problemStageBottomLeft

    pillars :: [Picture]
    pillars = map drawPillar problem.problemPillars
    drawPillar (Pillar (px, py) radius) =
        toAbs
            (fromIntegral px)
            (fromIntegral py)
            $ Color chartreuse
            $ Circle (fromIntegral radius)

    musicians :: [Picture]
    musicians = V.toList $ V.imap drawMusician $ V.convert $ solution.solutionPlacements
    drawMusician musician (x, y) =
        toAbs
            (fromIntegral x)
            (fromIntegral y)
            $ Color musicianColor
            $ Circle 10
      where
        instrument = problem.problemMusicians UV.! (musician `mod` UV.length problem.problemMusicians)
        musicianColor = musicianColors !! (instrument `mod` length musicianColors)

    attendees :: [Picture]
    attendees = map drawAttendee problem.problemAttendees
    drawAttendee :: Attendee -> Picture
    drawAttendee attendee =
        toAbs (fromIntegral attendee.attendeeX) (fromIntegral attendee.attendeeY) $
            Circle 3

musicianColors :: [Color]
musicianColors =
    [greyN 0.7, black, red, green, blue, yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]
