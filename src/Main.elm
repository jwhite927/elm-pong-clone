module Main exposing (..)

import Playground exposing (..)

-- UPDATE

update : Computer -> State -> State
update computer memory =

    let
        vy = 10 * toY computer.keyboard
        y = memory.p1.y + vy
        bvx = if hittingPaddle memory (whichSide memory) then
                case whichSide memory of
                    Left -> 3
                    Right -> -3
              else memory.bvx
        bvy = if hittingTorB computer memory.by then negate memory.bvy else memory.bvy
        p1 = memory.p1
    in
        { memory
        | p1 = { p1  | y = (max computer.screen.bottom y), vy = vy }
        , bx = memory.bx + bvx
        , by = memory.by + bvy
        , bvx = bvx
        , bvy = bvy
        }


hittingTorB computer ballY =
    ballY + 10 > computer.screen.top
    || ballY - 10 < computer.screen.bottom

timeToCheck memory side =
    case side of
        Left  ->
            memory.bx < memory.p1.x + 20
        Right ->
            memory.bx > memory.p2.x - 20

type Side
    = Left
    | Right

whichSide memory =
    if memory.bx < 0 then Left else Right

hittingPaddle memory side =
    timeToCheck memory side
    && case side of
       Left ->
           memory.by - 10 < memory.p1.y + 45
           && memory.by + 10 > memory.p1.y - 45
       Right ->
           memory.by - 10 < memory.p2.y + 45
           && memory.by + 10 > memory.p2.y - 45

-- VIEW

view: Computer -> State -> List Shape
view computer memory =
    let
        w = computer.screen.width
        h = computer.screen.height
    in
        [ rectangle (rgb 0 0 0) w h
        , rectangle (rgb 250 250 250) 20 90
            |> move memory.p1.x memory.p1.y
        , rectangle (rgb 250 250 250) 20 90
            |> move memory.p2.x memory.p2.y
        , square (rgb 250 250 250) 20
            |> move memory.bx memory.by
         ]

-- MAIN

type alias Paddle =
    { x: Float
    , y : Float
    , vy: Float
    }

initLeftPaddle =
    Paddle -300 0 0

initRightPaddle =
    Paddle 300 0 0

type alias State =
    { p1 : Paddle
    , p2 : Paddle
    , bx : Float
    , by : Float
    , bvx : Float
    , bvy : Float
    }

initState : State
initState =
    State initLeftPaddle initRightPaddle 0 0 -3 0

main =
    game view update initState


