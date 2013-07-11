module Elmzee where

import Keyboard
import Window

-- user input

type UserInput = 
  { space : Bool
  }

userInput : Signal UserInput
userInput = UserInput <~ Keyboard.space

data Input = Input Float UserInput


-- model

data State = Waiting | Rolling

type GameState = 
  { state : State
  , rollingSince : Float
  }

defaultGame : GameState
defaultGame = 
  { state = Waiting
  , rollingSince = 0.0
  }


-- update

stepGame : Input -> GameState -> GameState
stepGame (Input delta userInput) gameState = 
  { gameState | state <- case gameState.state of 
                           Waiting -> if userInput.space then Rolling else Waiting
                           Rolling -> if gameState.rollingSince > 1000 then Waiting else Rolling
              , rollingSince <- case gameState.state of
                                  Waiting -> 0.0
                                  Rolling -> gameState.rollingSince + delta
  }


-- display

elmzeeRed   = rgb 193 23 39
elmzeeWhite = rgb 255 254 255

txt = text . Text.height 4 . monospace . Text.color elmzeeRed . toText

stateMsg : GameState -> String
stateMsg gameState = case gameState.state of
                       Waiting -> "Press SPACE to roll dice"
                       Rolling -> "Rolling some dice"

display : (Int,Int) -> GameState -> Element
display (w,h) gameState = color elmzeeWhite <| container w h middle <| txt (stateMsg gameState)


-- main

delta = fps 45
input = sampleOn delta (lift2 Input delta userInput)

gameState = foldp stepGame defaultGame input

main = lift2 display Window.dimensions gameState
