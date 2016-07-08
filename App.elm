import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)



main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Model =
    { historyIndex : Int
    , history : (List (List Position))
    }


init : ( Model, Cmd Msg )
init =
  ( Model 0 [[]], Cmd.none)



-- UPDATE


type Msg
    = Click Position
    | RemoveDot Position
    | Undo
    | Redo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({history, historyIndex} as model) =
  case msg of
    Click xy ->
       ({ model |
         history = (List.take (historyIndex + 1) history) ++ [(getCurrent model) ++ [xy]]
         , historyIndex = historyIndex + 1
         }, Cmd.none)
    RemoveDot xy ->
       ({ model |
         history = (List.take
           (historyIndex + 1) history) ++ [List.filter
             (\dot -> dot /= xy)
             (getCurrent model)]
         , historyIndex = historyIndex + 1
         }, Cmd.none)
    Undo->
       ({ model |
         historyIndex = (Basics.max 0 (historyIndex - 1))}, Cmd.none)
    Redo->
       ({ model |
         historyIndex = (Basics.min ((List.length history) - 1) (historyIndex + 1))}, Cmd.none)

getCurrent : Model -> (List Position)
getCurrent model =
  case (List.head (List.drop model.historyIndex model.history)) of
    Just val -> val
    Nothing -> []

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
  div []
  [
    (div
      [ onAdd
      , style
        [ "background-color" => "#eeeeee"
        , "width" => "800px"
        , "height" => "400px"
        ]
      ]
      (List.map
      (\dot -> (div
          [
            (onClick (RemoveDot dot)),
            style
              [ "background-color" => "#000000"
              , "cursor" => "move"
              , "width" => "20px"
              , "height" => "20px"
              , "margin-left" => "-10px"
              , "margin-top" => "-10px"
              , "border-radius" => "10px"
              , "position" => "absolute"
              , "left" => px dot.x
              , "top" => px dot.y
              ]
          ]
          [ ])
        )
        (getCurrent model)))
        , (text ("index:" ++ toString model.historyIndex))
        , (button [ onClick Undo ] [ text "Undo" ])
        , (button [ onClick Redo ] [ text "Redo" ])
        ]


px : Int -> String
px number =
  toString number ++ "px"

onAdd : Attribute Msg
onAdd =
  on "click" (Json.map Click Mouse.position)
