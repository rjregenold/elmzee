module Elmzee where

import Keyboard
import Window

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type UserInput = 
  { space : Bool
  }

userInput : Signal UserInput
userInput = UserInput <~ Keyboard.space

data Input = Input Float UserInput



{-- Part 2: Model the game ----------------------------------------------------

What information do you need to represent the entire game?

Tasks: Redefine `GameState` to represent your particular game.
       Redefine `defaultGame` to represent your initial game state.

For example, if you want to represent many objects that just have a position,
your GameState might just be a list of coordinates and your default game might
be an empty list (no objects at the start):

    type GameState = { objects : [(Float,Float)] }
    defaultGame = { objects = [] }

------------------------------------------------------------------------------}

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



{-- Part 3: Update the game ---------------------------------------------------

How does the game step from one state to another based on user input?

Task: redefine `stepGame` to use the UserInput and GameState
      you defined in parts 1 and 2. Maybe use some helper functions
      to break up the work, stepping smaller parts of the game.

------------------------------------------------------------------------------}

stepGame : Input -> GameState -> GameState
stepGame (Input delta userInput) gameState = 
  { gameState | state <- case gameState.state of 
                           Waiting -> if userInput.space then Rolling else Waiting
                           Rolling -> if gameState.rollingSince > 1000 then Waiting else Rolling
              , rollingSince <- case gameState.state of
                                  Waiting -> 0.0
                                  Rolling -> gameState.rollingSince + delta
  }



{-- Part 4: Display the game --------------------------------------------------

How should the GameState be displayed to the user?

Task: redefine `display` to use the GameState you defined in part 2.

------------------------------------------------------------------------------}

elmzeeRed   = rgb 193 23 39
elmzeeWhite = rgb 255 254 255

txt = text . Text.height 4 . monospace . Text.color elmzeeRed . toText

stateMsg : GameState -> String
stateMsg gameState = case gameState.state of
                       Waiting -> "Press SPACE to roll dice"
                       Rolling -> "Rolling some dice"

display : (Int,Int) -> GameState -> Element
display (w,h) gameState = color elmzeeWhite <| container w h middle <| txt (stateMsg gameState)



{-- That's all folks! ---------------------------------------------------------

The following code puts it all together and show it on screen.

------------------------------------------------------------------------------}

delta = fps 45
input = sampleOn delta (lift2 Input delta userInput)

gameState = foldp stepGame defaultGame input

main = lift2 display Window.dimensions gameState
