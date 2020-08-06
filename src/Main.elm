module Main exposing (..)

import Playground exposing (..)
-- CONSTANTS

ballradius : Float
ballradius =
    10

rpaddleoffset : Float
rpaddleoffset =
    300

paddleheight : Float
paddleheight =
    90

paddlewidth : Float
paddlewidth =
    20

-- TYPES

-- Rectangular objects have edges, used for collisions
type alias Edges =
    { right : Float
    , left : Float
    , top : Float
    , bottom : Float
    }

-- Side describes which side of the screen we're on
type Side
    = Left
    | Right

makeEdges : Float -> Float -> Float -> Float -> Edges
makeEdges x y dx dy =
    Edges (x + dx) (x - dx) (y + dy) (y - dy)


-- The ball is square and has the most movement logic
type alias Ball =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , edges : Edges
    }

newBall : Float -> Float -> Float -> Float -> Ball
newBall x y vx vy =
    Ball x y vx vy (makeEdges x y ballradius ballradius)

moveBall : Ball -> Float -> Float -> Ball
moveBall ball bvx bvy =
    newBall (ball.x + bvx) (ball.y + bvy) bvx bvy


-- Paddles have a fixed x and only move in y
type alias Paddle =
    { x: Float
    , y : Float
    , edges : Edges
    }

initPaddle side =
    case side of
    Left ->
        Paddle (negate rpaddleoffset) 0 (makeEdges (negate rpaddleoffset) 0 (paddlewidth / 2) (paddleheight / 2)
    Right ->
        Paddle rpaddleoffset 0 (makeEdges rpaddleoffset 0 (paddlewidth / 2) (paddleheight / 2)

-- State holds the entire game state
type alias State =
    { paddle1 : Paddle
    , paddle2 : Paddle
    , ball : Ball
    }

initState : State
initState =
    -- State (initPaddle Left) (initPaddle Right) (newBall 0 0 -3 0)
    State (initPaddle Left) (initPaddle Right) (newBall 0 0 -3 0)



-- UPDATE

update : Computer -> State -> State
update computer memory =

    let
        y = memory.paddle1.y +  (computer.screen.height / 80) * toY computer.keyboard
        bvx = if hittingPaddle memory (whichSide memory) then
                case whichSide memory of
                    Left -> 3
                    Right -> -3
              else memory.ball.vx
        bvy = if hittingTorB computer memory.ball then negate memory.ball.vy else memory.ball.vy
        paddle1 = memory.paddle1
        ball = memory.ball
    in
        { memory
        | paddle1 = { paddle1  | y = (max computer.screen.bottom y) }
        , ball = moveBall ball bvx bvy
        }

hittingTorB : Computer -> Ball -> Bool
hittingTorB computer ball =
    ball.edges.top > computer.screen.top
    || ball.edges.bottom < computer.screen.bottom

timeToCheck memory side =
    case side of
        Left  ->
            memory.ball.edges.left < memory.paddle1.x + 10
            && memory.ball.edges.left > memory.paddle1.x
        Right ->
            memory.ball.edges.right > memory.paddle2.x - 10
            && memory.ball.edges.right < memory.paddle2.x

whichSide memory =
    if memory.ball.x < 0 then Left else Right

hittingPaddle memory side =
    timeToCheck memory side
    && case side of
       Left ->
           memory.ball.edges.left < memory.paddle1.y + 45
           && memory.ball.y+ 10 > memory.paddle1.y - 45
       Right ->
           memory.ball.y- 10 < memory.paddle2.y + 45
           && memory.ball.y+ 10 > memory.paddle2.y - 45

-- VIEW

view: Computer -> State -> List Shape
view computer memory =
    let
        w = computer.screen.width
        h = computer.screen.height
    in
        [ rectangle (rgb 0 0 0) w h
        , rectangle (rgb 250 250 250) 20 90
            |> move memory.paddle1.x memory.paddle1.y
        , rectangle (rgb 250 250 250) 20 90
            |> move memory.paddle2.x memory.paddle2.y
        , square (rgb 250 250 250) (ballradius * 2)
            |> move memory.ball.x memory.ball.y
         ]

-- MAIN

main =
    game view update initState


