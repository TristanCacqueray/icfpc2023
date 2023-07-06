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

import Data.Coerce (coerce)
import DearImGui
import DearImGui.Raw qualified
import RIO
import SDL hiding (Texture, textureHeight, textureWidth)
import SDL qualified

import Data.Massiv.Array (Ix2 ((:.)), Sz (..))
import Data.Massiv.Array qualified as Massiv
import Data.Massiv.Array.IO (Image)
import Data.Massiv.Array.Manifest (S, toStorableVector)
import Foreign qualified
import Foreign.C (CInt (..))
import Graphics.ColorModel qualified as CM
import Unsafe.Coerce (unsafeCoerce)

import Numeric.Noise.Perlin qualified as Perlin

renderNoise :: V2 Int -> Int -> Image S CM.RGB Word8
renderNoise (V2 x y) seed = Massiv.makeArray (Massiv.ParN 0) (Sz (x :. y)) go
  where
    go (i Massiv.:. j) =
        let v = Perlin.noiseValue perlinNoise (fromIntegral i, fromIntegral j, 0)
            uv = round (v * 255)
         in CM.PixelRGB uv uv uv
    octaves = 5
    scale = 0.05
    persistance = 0.5
    perlinNoise = Perlin.perlin seed octaves scale persistance

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

    let imgSize = V2 500 500

    let withTexture cb = do
            txt <- loadImage (renderNoise imgSize 1)
            cb txt

    seedValue <- newIORef 1
    mainGUI keyHandler withTexture \txt -> do
        withFullscreen do
            text "progcon demo"
            DearImGui.plotLines "samples" [sin (x / 10) | x <- [0 .. 64]]
            whenM (DearImGui.sliderInt "seed" seedValue 1 42) do
                seed <- readIORef seedValue
                putStrLn $ "Regen noise with " <> show seed
                loadData (renderNoise imgSize seed) txt
            drawTexture txt

data Texture = Texture
    { textureID :: GL.GLuint
    , textureWidth :: GL.GLsizei
    , textureHeight :: GL.GLsizei
    }
    deriving (Show)

textureSize :: Texture -> DearImGui.ImVec2
textureSize texture =
    DearImGui.ImVec2
        (fromIntegral $ texture.textureWidth)
        (fromIntegral $ texture.textureHeight)

-- | Create a texture pointer in GL memory.
create2DTexture :: Int -> Int -> IO Texture
create2DTexture width height =
    Foreign.alloca \ptr -> do
        GL.glGenTextures 1 ptr
        tID <- Foreign.peek ptr
        pure
            Texture
                { textureID = tID
                , textureWidth = fromIntegral width
                , textureHeight = fromIntegral height
                }

bindTexture :: Texture -> Foreign.Ptr GL.GLubyte -> IO ()
bindTexture texture dataPtr = do
    GL.glEnable GL.GL_TEXTURE_2D
    GL.glBindTexture GL.GL_TEXTURE_2D texture.textureID

    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MIN_FILTER GL.GL_LINEAR
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_MAG_FILTER GL.GL_LINEAR
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_S GL.GL_REPEAT
    GL.glTexParameteri GL.GL_TEXTURE_2D GL.GL_TEXTURE_WRAP_T GL.GL_REPEAT

    GL.glTexImage2D
        GL.GL_TEXTURE_2D
        0
        GL.GL_RGB
        texture.textureWidth
        texture.textureHeight
        0
        GL.GL_RGB
        GL.GL_UNSIGNED_BYTE
        (Foreign.castPtr dataPtr)

loadImage :: Image Massiv.S CM.RGB Word8 -> IO Texture
loadImage arr = do
    texture <- create2DTexture x y
    loadData arr texture
    pure texture
  where
    Sz (x :. y) = Massiv.size arr

loadData :: Image Massiv.S CM.RGB Word8 -> Texture -> IO ()
loadData arr texture =
    SV.unsafeWith vec \ptr -> do
        bindTexture texture (Foreign.castPtr ptr)
  where
    vec :: SV.Vector (CM.Pixel CM.RGB Word8)
    vec = toStorableVector arr

{-# ANN drawTexture ("HLint: ignore Avoid lambda" :: String) #-}
drawTexture :: Texture -> IO ()
drawTexture texture =
    Foreign.with (textureSize texture) \sizePtr ->
        Foreign.with (DearImGui.ImVec2 0 0) \uv0Ptr ->
            Foreign.with (DearImGui.ImVec2 1 1) \uv1Ptr ->
                Foreign.with (DearImGui.ImVec4 1 1 1 1) \tintColPtr ->
                    Foreign.with (DearImGui.ImVec4 1 1 1 1) \borderColPtr ->
                        DearImGui.Raw.image openGLtextureID sizePtr uv0Ptr uv1Ptr tintColPtr borderColPtr
  where
    openGLtextureID = Foreign.intPtrToPtr $ fromIntegral texture.textureID

usage :: IO ()
usage =
    getArgs >>= \case
        [] -> pure ()
        _ -> error "usage: progcon-gui"

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
