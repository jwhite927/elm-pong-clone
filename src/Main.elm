module Main exposing (..)

import Debug exposing (toString)
import Playground exposing (..)



-- CONSTANTS


ballradius : Float
ballradius =
    10

ballSpeed: Float
ballSpeed =
    6

rpaddleoffset : Float
rpaddleoffset =
    300


paddleheight : Float
paddleheight =
    120


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


makeEdges : Float -> Float -> Float -> Float -> Edges
makeEdges x y dx dy =
    Edges (x + dx) (x - dx) (y + dy) (y - dy)



-- Side describes which side of the screen we're on


type Side
    = Left
    | Right



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
    { x : Float
    , y : Float
    , edges : Edges
    }


makePaddle : Side -> Float -> Paddle
makePaddle side y =
    case side of
        Left ->
            Paddle (negate rpaddleoffset) y (makeEdges (negate rpaddleoffset) y (paddlewidth / 2) (paddleheight / 2))

        Right ->
            Paddle rpaddleoffset y (makeEdges rpaddleoffset y (paddlewidth / 2) (paddleheight / 2))



-- Score is the count of points scored by each player


type alias Score =
    { player1score : Float
    , player2score : Float
    }



-- State hold all the game memory


type alias State =
    { paddle1 : Paddle
    , paddle2 : Paddle
    , ball : Ball
    , score : Score
    }


initScore : Score
initScore =
    Score 0 0


initState : State
initState =
    State (makePaddle Left 0) (makePaddle Right 0) (newBall 0 0 ballSpeed 3) initScore


resetState : Score -> State
resetState score =
    State (makePaddle Left 0) (makePaddle Right 0) (newBall 0 0 ballSpeed 3) score



-- UPDATE


type Scoring
    = Maybe Side


playerScoring memory boundary =
    if memory.ball.edges.left < negate boundary then
        Just Left

    else if memory.ball.edges.right > boundary then
        Just Right

    else
        Nothing


update : Computer -> State -> State
update computer memory =
    case playerScoring memory (fieldSize computer / 2) of
        Just Left ->
            resetState (Score (memory.score.player1score + 1) memory.score.player2score)

        Just Right ->
            resetState (Score memory.score.player1score (memory.score.player2score + 1))

        Nothing ->
            let
                y =
                    memory.paddle1.y + (computer.screen.height / 80) * toY computer.keyboard

                bvx =
                    if hittingPaddle memory (whichSide memory) then
                        case whichSide memory of
                            Left ->
                                ballSpeed

                            Right ->
                                negate ballSpeed

                    else
                        memory.ball.vx

                bvy =
                    if hittingTorB computer memory.ball then
                        negate memory.ball.vy

                    else
                        memory.ball.vy

                paddle1 =
                    memory.paddle1

                ball =
                    memory.ball

                boundary =
                    fieldSize computer / 2
            in
            { memory
                | paddle1 =
                    makePaddle Left
                        (if paddle1.edges.top > (fieldSize computer / 2) then
                            boundary - paddleheight / 2

                         else if paddle1.edges.bottom < negate (fieldSize computer / 2) then
                            negate boundary + paddleheight / 2

                         else
                            y
                        )
                , ball = moveBall ball bvx bvy
            }


hittingTorB : Computer -> Ball -> Bool
hittingTorB computer ball =
    let
        boundary =
            fieldSize computer / 2
    in
    ball.edges.top
        > boundary
        || ball.edges.bottom
        < negate boundary


timeToCheck : State -> Side -> Bool
timeToCheck memory side =
    case side of
        Left ->
            memory.ball.edges.left
                < memory.paddle1.edges.right
                && memory.ball.edges.left
                > memory.paddle1.x

        Right ->
            memory.ball.edges.right
                > memory.paddle2.edges.left
                && memory.ball.edges.right
                < memory.paddle2.x


whichSide : State -> Side
whichSide memory =
    if memory.ball.x < 0 then
        Left

    else
        Right


hittingPaddle : State -> Side -> Bool
hittingPaddle memory side =
    timeToCheck memory side
        && (case side of
                Left ->
                    memory.ball.edges.bottom
                        < memory.paddle1.edges.top
                        && memory.ball.edges.top
                        > memory.paddle1.edges.bottom

                Right ->
                    memory.ball.edges.bottom
                        < memory.paddle2.edges.top
                        && memory.ball.edges.top
                        > memory.paddle2.edges.bottom
           )



-- VIEW


fieldSize computer =
    min computer.screen.width computer.screen.height * 0.8


view : Computer -> State -> List Shape
view computer memory =
    let
        w =
            computer.screen.width

        h =
            computer.screen.height

        rightScoreX =
            computer.screen.width * (1 / 8)

        scoreY =
            computer.screen.height * (7 / 16)
    in
    [ rectangle (rgb 68 68 68) w h
    , square black (fieldSize computer)
    , rectangle white paddlewidth paddleheight
        |> move memory.paddle1.x memory.paddle1.y
    , rectangle white paddlewidth paddleheight
        |> move memory.paddle2.x memory.paddle2.y
    , square white (ballradius * 2)
        |> move memory.ball.x memory.ball.y
    , rectangle white 5 70
        |> move 0 scoreY
    , words white (toString memory.score.player1score)
        |> scale 4
        |> move (negate rightScoreX) scoreY
    , words white (toString memory.score.player2score)
        |> scale 4
        |> move rightScoreX scoreY
    ]



-- MAIN


main =
    game view update initState
