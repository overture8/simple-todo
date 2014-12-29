import Graphics.Element (Element, container, midTop)
import String
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Html.Lazy (lazy, lazy2)
import Json.Decode as Json
import Signal
import List
import Window
import Maybe

--- MODEL ---


type alias State =
  { tasks       : List Task
  , field       : String
  , uid         : Int
  , visibility  : String
  }

type alias Task =
  { description : String
  , id          : Int
  }

newTask : String -> Int -> Task
newTask desc id = 
  { description = desc 
  , id = id
  }

emptyState : State
emptyState =
  { tasks = []
  , visibility = "All"
  , field = ""
  , uid = 0
  }


--- UPDATE ---


type Update
  = NoOp
  | UpdateField String
  | Add

step : Update -> State -> State
step update state =
  case update of
    NoOp -> state

    UpdateField str -> { state | field <- str }
  
    Add -> 
      { state | uid <- state.uid + 1
              , field <- ""
              , tasks <- if String.isEmpty state.field
                            then state.tasks
                            else state.tasks ++ [newTask state.field state.uid]
      }


--- VIEW ---


view : State -> Html
view state = 
  div
    [ class "todomvc-wrapper" ]
    [ section
      [ id "todoapp" ]
      [ lazy taskEntry state.field
      , lazy2 taskList state.visibility state.tasks
      ]
    ]

onEnter : Signal.Message -> Attribute
onEnter message = 
  on "keydown"
    (Json.customDecoder keyCode is13)
    (always message)

is13 : Int -> Result String ()
is13 code = 
  if code == 13 then Ok () else Err "not the right key"

taskEntry : String -> Html
taskEntry task = 
  header 
    [ id "header" ]
    [ h1 [] [ text "todos" ]
    , input 
      [ id "new-todo" 
      , placeholder "What needs to be done?"
      , autofocus True
      , value task
      , name "newTodo"
      , on "input" targetValue (Signal.send updates << UpdateField)
      , onEnter (Signal.send updates Add)
      ]
      []
    ]

taskList : String -> List Task -> Html
taskList visibilty tasks = 
  section
    [ id "main" ]
    [ ul
      [ id "todo-list" ]
      (List.map todoItem tasks)
    ]

todoItem : Task -> Html
todoItem todo =
  li 
    [class "todo-item"]

--- INPUTS ---


main : Signal Element
main = Signal.map2 scene state Window.dimensions

scene : State -> (Int,Int) -> Element
scene state (w,h) = container w h midTop (toElement 550 h (view state))

state : Signal State
state = Signal.foldp step emptyState (Signal.subscribe updates)

updates : Signal.Channel Update
updates = Signal.channel NoOp

