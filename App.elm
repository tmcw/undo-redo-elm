module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onWithOptions)
import Json.Decode as Json exposing ((:=))
import Mouse exposing (Position)
import Maybe exposing (withDefault)


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
    , history : List (List Position)
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 [ [] ], Cmd.none )



-- UPDATE


type Msg
    = Click Position
    | RemoveDot Position
    | Undo
    | Redo


nextModel : Model -> List Position -> ( Model, Cmd Msg )
nextModel model next =
    ( { model
        | history = (List.take (model.historyIndex + 1) model.history) ++ [ next ]
        , historyIndex = model.historyIndex + 1
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ history, historyIndex } as model) =
    case msg of
        Click xy ->
            nextModel model ((getCurrent model) ++ [ xy ])

        RemoveDot xy ->
            nextModel model (List.filter ((/=) xy) (getCurrent model))

        Undo ->
            ( { model
                | historyIndex = (Basics.max 0 (historyIndex - 1))
              }
            , Cmd.none
            )

        Redo ->
            ( { model
                | historyIndex = (Basics.min ((List.length history) - 1) (historyIndex + 1))
              }
            , Cmd.none
            )


getCurrent : Model -> List Position
getCurrent model =
    withDefault []
        (model.history
            |> List.drop model.historyIndex
            |> List.head
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


(=>) =
    (,)


onClickStop : msg -> Attribute msg
onClickStop message =
    onWithOptions "click" { stopPropagation = True, preventDefault = False } (Json.succeed message)


buttonStyle : Bool -> List ( String, String )
buttonStyle disabled =
    [ "border-radius" => "5px"
    , "margin" => "5px"
    , "border-width" => "0"
    , "background"
        => if disabled then
            "#aaa"
           else
            "#2969B0"
    , "color" => "#fff"
    ]


view : Model -> Html Msg
view model =
    div []
        [ (div
            [ onAdd
            , style
                [ "background-color" => "#eeeeee"
                , "width" => "600px"
                , "height" => "200px"
                ]
            ]
            (List.map
                (\dot ->
                    (div
                        [ (onClickStop (RemoveDot dot))
                        , style
                            [ "background-color" => "#EB6B56"
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
                        []
                    )
                )
                (getCurrent model)
            )
          )
        , (button
            [ onClick Undo
            , style (buttonStyle (model.historyIndex == 0))
            ]
            [ text "Undo" ]
          )
        , (button [ onClick Redo, style (buttonStyle (model.historyIndex == (List.length model.history) - 1)) ] [ text "Redo" ])
        ]


px : Int -> String
px number =
    toString number ++ "px"


onAdd : Attribute Msg
onAdd =
    on "click" (Json.map Click Mouse.position)
