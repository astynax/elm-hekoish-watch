module Main exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Date exposing (Date)
import Element exposing (toHtml)
import Html exposing (Html, program)
import List exposing (filterMap)
import Platform.Cmd
import Platform.Sub
import Task
import Time exposing (every, second)
import Window exposing (Size)


type Msg
    = Resize Size
    | Tick Date


type alias Model =
    ( ( Int, Int ), Date )


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = always <| subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( ( ( 0, 0 ), Date.fromTime 0 )
    , Cmd.batch
        [ Task.perform Resize Window.size
        , Task.perform Tick Date.now
        ]
    )


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Window.resizes Resize
        , every (10 * second) (Tick << Date.fromTime)
        ]


view : Model -> Html Msg
view ( ( w, h ), date ) =
    let
        width =
            toFloat w

        height =
            toFloat h
    in
        toHtml <|
            collage w
                h
                [ filled black <| rect width height
                , clock (min width height) ( Date.hour date, Date.minute date )
                ]



{-
   "LED" layout and meaning:
   (inspired from http://www.tokyoflash.com/en/watch_museum/tokyoflash/heko/)

         +12h
    9h +1h +1h 3h
          6h
   +1m +1m +1m +1m
         <  >       <- there is no +60m in our world
   45m +5m +5m 15m
         30m

   For example "17:43" looks like:
          g         +12
        r   r   g   +1 +1 3
                    = 3 +12 +1 +1 = 17 hours
    y   y   y       +1 +1 +1

        r       g   +5 15
          g         30
                    = max(30, 15) +5 +1 +1 +1 = 43
-}


clock : Float -> ( Int, Int ) -> Form
clock size ( h, m ) =
    let
        ledSize =
            size / 30

        d1_10 =
            size / 10

        d3_10 =
            3 * d1_10

        d1_5 =
            2 * d1_10

        d2_5 =
            2 * d1_5

        led p c1 c2 x y =
            if p then
                Just
                    (move ( x, y ) <|
                        group
                            [ filled c1 <| circle ledSize
                            , filled c2 <| circle <| ledSize * 0.8
                            ]
                    )
            else
                Nothing

        gtt12 =
            h >= 12

        hh =
            h % 12

        h3 =
            h % 3

        m5 =
            (m % 15) // 5

        m1 =
            m % 5
    in
        group <|
            filterMap identity <|
                [ led (hh >= 3) green lightGreen d3_10 d3_10
                , led (hh >= 9) green lightGreen -d3_10 d3_10
                , led (hh >= 6) green lightGreen 0 d1_5
                , led (gtt12) green lightGreen 0 d2_5
                , led (m >= 15) green lightGreen d3_10 -d3_10
                , led (m >= 45) green lightGreen -d3_10 -d3_10
                  --, led (m >= 60) green  lightGreen    0     -d1_5  -- 60min newer happen
                , led (m >= 30) green lightGreen 0 -d2_5
                , led (h3 > 0) red lightRed -d1_10 d3_10
                , led (h3 > 1) red lightRed d1_10 d3_10
                , led (m5 > 0) red lightRed -d1_10 -d3_10
                , led (m5 > 1) red lightRed d1_10 -d3_10
                , led (m1 > 0) yellow lightYellow -d3_10 0
                , led (m1 > 1) yellow lightYellow -d1_10 0
                , led (m1 > 2) yellow lightYellow d1_10 0
                , led (m1 > 3) yellow lightYellow d3_10 0
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( size, date ) =
    case msg of
        Resize s ->
            ( ( ( s.width, s.height ), date ), Cmd.none )

        Tick d ->
            ( ( size, d ), Cmd.none )
