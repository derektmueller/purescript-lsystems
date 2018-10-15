module Koch3 where

import Prelude
import Effect (Effect)
import Graphics.Canvas as C
import Math as Math

type Angle = Number
data Alphabet = L Angle | R Angle | F
type Sentence = Array Alphabet
type State = 
  { x :: Number
  , y :: Number
  , theta :: Number
  }

lAngle :: Angle
lAngle = Math.pi / 4.0

rAngle :: Angle
rAngle = Math.pi / 5.0

initial :: Sentence
initial = 
  [ F
  , R rAngle
  , R rAngle
  , F
  , R rAngle
  , R rAngle
  , F
  , R rAngle
  , R rAngle
  ]

productions :: Alphabet -> Sentence
productions F = 
  [ F
  , L lAngle
  , F
  , R rAngle
  , R rAngle
  , F
  , L lAngle
  , F
  ]
productions l = pure l

initialState :: State
initialState = { x: 120.0, y: 200.0, theta: 0.0 }

mkCanvasInterpreter 
  :: C.Context2D -> State -> Alphabet -> Effect State
mkCanvasInterpreter ctx = canvasInterpreter
  where
    canvasInterpreter :: State -> Alphabet -> Effect State
    canvasInterpreter state (L t) = 
      pure $ state { theta = state.theta - t }
    canvasInterpreter state (R t) = 
      pure $ state { theta = state.theta + t }
    canvasInterpreter state F = do
      let x = state.x + Math.cos state.theta * 10.5
          y = state.y + Math.sin state.theta * 10.5
      C.lineTo ctx x y
      pure { x, y, theta: state.theta }

