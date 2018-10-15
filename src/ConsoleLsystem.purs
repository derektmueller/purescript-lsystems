module ConsoleLsystem where

import Prelude
import Effect (Effect)
import Lsystem (lsystem, interpret)
import Sierpinski as K

main :: Effect Unit
main = void do
  let generated = lsystem K.initial K.productions 5
      interpreter = K.consoleInterpreter

  interpret generated interpreter K.initialState
