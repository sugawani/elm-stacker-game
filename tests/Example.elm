module Example exposing (..)

import Expect exposing (Expectation)
import Main exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "test"
        [ describe "getLightColor"
            [ test "LightOnを渡すとredが返る"
                (\_ ->
                    let
                        colorString =
                            getLightColor LightOn
                    in
                    Expect.equal "red" colorString
                )
            , test "LightOffを渡すとwhiteが返る"
                (\_ ->
                    let
                        colorString =
                            getLightColor LightOff
                    in
                    Expect.equal "white" colorString
                )
            ]
        , describe "invertMode"
            [ test "Rightを渡すとLeftが返る"
                (\_ ->
                    let
                        moveMode =
                            invertMoveMode Right
                    in
                    Expect.equal Left moveMode
                )
            , test "Leftを渡すとRightが返る"
                (\_ ->
                    let
                        moveMode =
                            invertMoveMode Left
                    in
                    Expect.equal Right moveMode
                )
            ]
        , describe "getOperator"
            [ test "Leftを渡すと(-)が返る"
                (\_ ->
                    let
                        operator =
                            getOperator Left
                    in
                    Expect.equal (-) operator
                )
            , test "Rightを渡すと(+)が返る"
                (\_ ->
                    let
                        operator =
                            getOperator Right
                    in
                    Expect.equal (+) operator
                )
            ]
        ]
