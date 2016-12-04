module App exposing (..)

import Html exposing (Html, text, div)
import Window exposing (size)
import Task
import Animation exposing (px)
import Svg exposing (svg, rect)
import Svg.Attributes
    exposing
        ( width
        , height
        , viewBox
        , x
        , y
        , fill
        , stroke
        , strokeWidth
        )


type alias Model =
    { windowSize : Window.Size
    , square : Animation.State
    }


initialSquare : Animation.State
initialSquare =
    beginInitialWalk <|
        Animation.style <|
            [ Animation.x 2.0
            , Animation.y 2.0
            ]


init : ( Model, Cmd Msg )
init =
    ( Model { width = 0, height = 0 } initialSquare
    , Task.perform (\size -> Resize size) Window.size
    )


type Msg
    = NoOp
    | Resize Window.Size
    | Animate Animation.Msg
    | StartWalking


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Resize size ->
            ( { model | windowSize = size }
            , Cmd.none
            )

        Animate animMsg ->
            ( { model
                | square = Animation.update animMsg model.square
              }
            , Cmd.none
            )

        StartWalking ->
            ( { model
                | square =
                    Animation.interrupt
                        [ Animation.loop
                            [ Animation.to
                                [ Animation.x 500 ]
                            ]
                        ]
                        model.square
              }
            , Cmd.none
            )



-- beginInitialWalk : Model -> Model


beginInitialWalk : Animation.State -> Animation.State
beginInitialWalk squareStyle =
    Animation.interrupt
        [ Animation.loop
            [ Animation.to
                [ Animation.x 500 ]
            , Animation.set
                [ Animation.x 0 ]
            ]
        ]
        squareStyle


view : Model -> Html Msg
view model =
    let
        { windowSize } =
            model

        viewBoxsize =
            "0 0 " ++ (toString windowSize.width) ++ " " ++ (toString windowSize.height)
    in
        svg
            [ width <| toString <| windowSize.width
            , height <| toString <| windowSize.height
            , viewBox viewBoxsize
            ]
            [ rect
                (Animation.render model.square
                    ++ [ width "50"
                       , height "50"
                       , fill "none"
                       , stroke "#f88"
                       , strokeWidth "3"
                       ]
                )
                []
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes (\size -> Resize size)
        , Animation.subscription Animate [ model.square ]
        ]
