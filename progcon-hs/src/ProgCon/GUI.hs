module ProgCon.GUI where

import Control.Monad.Managed (managed, managed_, runManaged)
import Data.Vector.Storable qualified as SV
import Data.Vector.Storable.Mutable qualified as V
import DearImGui.OpenGL3 qualified
import DearImGui.SDL qualified
import DearImGui.SDL.OpenGL qualified
import Graphics.GL qualified as GL
import System.Environment (getArgs)
import Witch (from)

import DearImGui
import RIO
import SDL hiding (Texture)

usage :: IO ()
usage =
    getArgs >>= \case
        [] -> pure ()
        _ -> error "usage: progcon-gui"

main :: IO ()
main = do
    usage

    initializeAll

    let keyHandler event
          | isQuit = pure True
          | otherwise = pure False
         where
            keyCode = case eventPayload event of
                KeyboardEvent ke | ke.keyboardEventKeyMotion == Pressed -> Just ke.keyboardEventKeysym.keysymScancode
                _ -> Nothing
            isQuit =
                SDL.eventPayload event == SDL.QuitEvent
                    || keyCode == Just ScancodeEscape

    let withTexture cb = do
            cb ()

    mainGUI keyHandler withTexture \() -> withFullscreen do
        text "progcon demo"
        DearImGui.plotLines "samples" [sin (x / 10) | x <- [0..64]]

-- sdl bootstrap adapted from the dear-imgui readme.
mainGUI :: (Event -> IO Bool) -> _ -> (_ -> IO ()) -> IO ()
mainGUI eventHandler withTextures renderUI = do
    runManaged do
        window <- do
            let title = "simple-dsp-demo"
            let config = defaultWindow{windowGraphicsContext = OpenGLContext defaultOpenGL}
            managed $ bracket (createWindow title config) destroyWindow
        glContext <- managed $ bracket (glCreateContext window) glDeleteContext
        _ <- managed $ bracket createContext destroyContext
        _ <- managed_ $ bracket_ (DearImGui.SDL.OpenGL.sdl2InitForOpenGL window glContext) DearImGui.SDL.sdl2Shutdown
        _ <- managed_ $ bracket_ DearImGui.OpenGL3.openGL3Init DearImGui.OpenGL3.openGL3Shutdown

        liftIO $ withTextures \textures -> mainLoop window (renderUI textures) eventHandler

mainLoop :: Window -> IO () -> (Event -> IO Bool) -> IO ()
mainLoop window renderUI eventHandler = unlessQuit do
    DearImGui.OpenGL3.openGL3NewFrame
    DearImGui.SDL.sdl2NewFrame
    DearImGui.newFrame
    renderUI
    GL.glClear GL.GL_COLOR_BUFFER_BIT
    DearImGui.render
    DearImGui.OpenGL3.openGL3RenderDrawData =<< getDrawData
    SDL.glSwapWindow window
    mainLoop window renderUI eventHandler
  where
    unlessQuit action = do
        shouldQuit <- traverse eventHandler =<< DearImGui.SDL.pollEventsWithImGui
        unless (or shouldQuit) action
