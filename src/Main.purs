module Main where

import Prelude
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Graphics.Canvas as C
import Web.HTML (window)
import Web.HTML.Window (document, innerWidth, innerHeight)
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Sierpinski as K
import Lsystem (interpret, lsystem)

renderPage :: C.Context2D -> Effect Unit
renderPage ctx = void do
  C.setFillStyle ctx "#0000FF"

  let generated = lsystem K.initial K.productions 5
      interpreter = K.mkCanvasInterpreter ctx

  C.strokePath ctx do
    C.moveTo ctx K.initialState.x K.initialState.y
    interpret generated interpreter K.initialState

main :: Effect Unit
main = void $ unsafePartial do
  win <- window
  doc <- document win

  canvasHeight <- innerHeight win
  canvasWidth <- innerWidth win

  Just canvas <- C.getCanvasElementById "main"

  C.setCanvasDimensions canvas {
    width: toNumber canvasWidth, height: toNumber canvasHeight
  }

  ctx <- C.getContext2D canvas

  renderPage ctx
