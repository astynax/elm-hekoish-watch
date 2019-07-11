module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Collage exposing (..)
import Collage.Layout
import Collage.Render
import Color exposing (black, green, lightGreen, lightRed, lightYellow, red, yellow)
import Html exposing (Html)
import List exposing (filterMap)
import Platform.Cmd
import Platform.Sub
import Task
import Time exposing (Posix)


type Msg
    = Resize Float Float
    | Tick Posix
    | Here Time.Zone


type alias Model =
    { width : Float
    , height : Float
    , date : Posix
    , zone : Time.Zone
    }


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , update = update
        , subscriptions = always subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    ( { width = 100
      , height = 100
      , date = Time.millisToPosix 0
      , zone = Time.utc
      }
    , Cmd.batch
        [ Task.perform (\vp -> Resize vp.viewport.width vp.viewport.height)
            Browser.Dom.getViewport
        , Task.perform Tick Time.now
        , Task.perform Here Time.here
        ]
    )


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Browser.Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Time.every (10 * 1000) Tick
        ]


view : Model -> Html Msg
view model =
    Collage.Render.svgBox
        ( model.width
        , model.height
        )
    <|
        Collage.Layout.stack
            [ clock (min model.width model.height)
                ( Time.toHour model.zone model.date
                , Time.toMinute model.zone model.date
                )
            , filled (uniform black) <| rectangle model.width model.height
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


clock : Float -> ( Int, Int ) -> Collage a
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
                    (shift ( x, y ) <|
                        group
                            [ filled (uniform c1) <| circle ledSize
                            , filled (uniform c2) <| circle <| ledSize * 0.8
                            ]
                    )

            else
                Nothing

        hh =
            modBy 12 h

        h3 =
            modBy 3 h

        m5 =
            modBy 15 m // 5

        m1 =
            modBy 5 m
    in
    group <|
        filterMap identity <|
            [ led (hh >= 3) green lightGreen d3_10 d3_10
            , led (hh >= 9) green lightGreen -d3_10 d3_10
            , led (hh >= 6) green lightGreen 0 d1_5
            , led (h >= 12) green lightGreen 0 d2_5
            , led (m >= 15) green lightGreen d3_10 -d3_10
            , led (m >= 45) green lightGreen -d3_10 -d3_10
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
update msg model =
    case msg of
        Resize w h ->
            ( { model | width = w, height = h }, Cmd.none )

        Tick d ->
            ( { model | date = d }, Cmd.none )

        Here z ->
            ( { model | zone = z }, Cmd.none )
