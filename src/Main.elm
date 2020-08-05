-- Walk around with the arrow keys. Press the UP arrow to jump!
-- Learn more about the playground here:
-- https://package.elm-lang.org/packages/evancz/elm-playground/latest/

module Main exposing (..)

import Playground exposing (..)


-- UPDATE

update computer player =
    let
        vy = 10 * toY computer.keyboard
        x = player.x
        y = player.y + vy
    in
        { x = x
        , y = max 0 y
        , vy = vy
        }


-- VIEW

view computer player =
    let
        w = computer.screen.width
        h = computer.screen.height
        b = computer.screen.bottom
    in
        [ rectangle (rgb 174 238 238) w h
        , rectangle (rgb 50 50 50) 20 90
            |> move player.x (b + 76 + player.y)
        , rectangle (rgb 50 50 50) 20 90
            |> move (player.x + 600) (b + 76 + player.y)
        ]


-- MAIN

main =
    game view update
    { x = -300
    , y = 0
    , vy = 0
    }

