module Sierpinski where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Graphics.Canvas as C
import Math as Math
import Data.List.Lazy.Types (List)
import Data.List.Lazy (fromFoldable)

data Alphabet = L | R | F | M
type Sentence = List Alphabet
type State = 
  { x :: Number
  , y :: Number
  , theta :: Number
  }

initial :: Sentence
initial = fromFoldable [M]

productions :: Alphabet -> Sentence
productions F = 
  fromFoldable [ F, L, M, L, F, R, M, R, F, R, M, R, F, L, M, L, F ]
productions M = 
  fromFoldable [ M, R, F, R, M, L, F, L, M, L, F, L, M, R, F, R, M ] 
productions l = pure l

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
    canvasInterpreter state _ = do
      let x = state.x + Math.cos state.theta * 0.5
          y = state.y + Math.sin state.theta * 0.5
      C.lineTo ctx x y
      pure { x, y, theta: state.theta }


consoleInterpreter :: State -> Alphabet -> Effect State
consoleInterpreter state L = do
  log "L" 
  pure state
consoleInterpreter state R = do
  log "R" 
  pure state
consoleInterpreter state F = do
  log "F" 
  pure state
consoleInterpreter state M = do
  log "M" 
  pure state

