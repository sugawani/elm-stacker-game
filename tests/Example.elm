module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
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
        ]
