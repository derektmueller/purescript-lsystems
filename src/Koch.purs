module Koch where

import Prelude
import Effect (Effect)
import Graphics.Canvas as C
import Math as Math

data Alphabet = L | R | F
type Sentence = Array Alphabet
type State = 
  { x :: Number
  , y :: Number
  , theta :: Number
  }

initial :: Sentence
initial = [F, R, R, F, R, R, F, R, R]

productions :: Alphabet -> Sentence
productions L = pure L
productions R = pure R
productions F = [F, L, F, R, R, F, L, F]

initialState :: State
initialState = { x: 120.0, y: 200.0, theta: 0.0 }

mkCanvasInterpreter 
  :: C.Context2D -> State -> Alphabet -> Effect State
mkCanvasInterpreter ctx = canvasInterpreter
  where
    canvasInterpreter :: State -> Alphabet -> Effect State
    canvasInterpreter state L = 
      pure $ state { theta = state.theta - Math.pi / 3.0 }
    canvasInterpreter state R = 
      pure $ state { theta = state.theta + Math.pi / 3.0 }
    canvasInterpreter state F = do
      let x = state.x + Math.cos state.theta * 1.5
          y = state.y + Math.sin state.theta * 1.5
      C.lineTo ctx x y
      pure { x, y, theta: state.theta }

