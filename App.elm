import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (on)
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
    , history : (List Position)
    }


init : ( Model, Cmd Msg )
init =
  ( Model 0 [(Position 200 200)], Cmd.none)



-- UPDATE


type Msg
    = Click Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({history} as model) =
  case msg of
    Click xy ->
       ({ model | history = history ++ [xy] }, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
  (div
    [ onClick
    , style
      [ "background-color" => "#eeeeee"
      , "width" => "800px"
      , "height" => "400px"
      ]
    ]
    (List.map
    (\dot -> (div
        [ style
            [ "background-color" => "#000000"
            , "cursor" => "move"
            , "width" => "20px"
            , "height" => "20px"
            , "border-radius" => "10px"
            , "position" => "absolute"
            , "left" => px dot.x
            , "top" => px dot.y
            , "color" => "white"
            , "display" => "flex"
            , "align-items" => "center"
            , "justify-content" => "center"
            ]
        ]
        [ ])
      ) model.history))


px : Int -> String
px number =
  toString number ++ "px"

onClick : Attribute Msg
onClick =
  on "click" (Json.map Click Mouse.position)
