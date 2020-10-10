module Example exposing (..)

import Expect exposing (Expectation)
import Main exposing (..)
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector


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
        , describe "invertMoveMode"
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
        , describe "moveLightPoints"
            [ describe "Right"
                [ test "yが+1される"
                    (\_ ->
                        let
                            movedLightPoints =
                                moveLightPoints [ { x = 0, y = 0 } ] Right
                        in
                        Expect.equal [ { x = 0, y = 1 } ] movedLightPoints
                    )
                , test "複数Pointがある場合はそれぞれyが+1される"
                    (\_ ->
                        let
                            lightPoints =
                                [ { x = 0, y = 0 }, { x = 0, y = 1 } ]

                            movedLightPoints =
                                moveLightPoints lightPoints Right

                            expectLightPoints =
                                [ { x = 0, y = 1 }, { x = 0, y = 2 } ]
                        in
                        Expect.equal expectLightPoints movedLightPoints
                    )
                , test "配列が空の場合は処理しない"
                    (\_ ->
                        let
                            movedLightPoints =
                                moveLightPoints [] Right
                        in
                        Expect.equal [] movedLightPoints
                    )
                ]
            , describe "Left"
                [ test "yが-1される"
                    (\_ ->
                        let
                            movedLightPoints =
                                moveLightPoints [ { x = 0, y = 0 } ] Left
                        in
                        Expect.equal [ { x = 0, y = -1 } ] movedLightPoints
                    )
                , test "複数Pointがある場合はそれぞれyが-1される"
                    (\_ ->
                        let
                            lightPoints =
                                [ { x = 0, y = 0 }, { x = 0, y = 1 } ]

                            movedLightPoints =
                                moveLightPoints lightPoints Left

                            expectLightPoints =
                                [ { x = 0, y = -1 }, { x = 0, y = 0 } ]
                        in
                        Expect.equal expectLightPoints movedLightPoints
                    )
                , test "配列が空の場合は処理しない"
                    (\_ ->
                        let
                            movedLightPoints =
                                moveLightPoints [] Left
                        in
                        Expect.equal [] movedLightPoints
                    )
                ]
            ]
        , describe "getMoveMode"
            [ describe "Right"
                [ test "配列が空の場合Rightが返る"
                    (\_ ->
                        let
                            moveMode =
                                getMoveMode Right []
                        in
                        Expect.equal Right moveMode
                    )
                , test "headのyが5の場合Leftが返る"
                    (\_ ->
                        let
                            lightPoints =
                                [ { x = 0, y = 5 }
                                , { x = 0, y = 0 }
                                ]

                            moveMode =
                                getMoveMode Right lightPoints
                        in
                        Expect.equal Left moveMode
                    )
                , test "head以外のyが5だとLeftにならない"
                    (\_ ->
                        let
                            lightPoints =
                                [ { x = 0, y = 4 }
                                , { x = 0, y = 5 }
                                , { x = 0, y = 5 }
                                ]

                            moveMode =
                                getMoveMode Right lightPoints
                        in
                        Expect.equal Right moveMode
                    )
                ]
            , describe "Left"
                [ test "配列が空の場合Leftが返る"
                    (\_ ->
                        let
                            moveMode =
                                getMoveMode Left []
                        in
                        Expect.equal Left moveMode
                    )
                , test "tailのyが1の場合Rightが返る"
                    (\_ ->
                        let
                            lightPoints =
                                [ { x = 0, y = 0 }
                                , { x = 0, y = 1 }
                                ]

                            moveMode =
                                getMoveMode Left lightPoints
                        in
                        Expect.equal Right moveMode
                    )
                , test "tail以外のyが1だとRightにならない"
                    (\_ ->
                        let
                            lightPoints =
                                [ { x = 0, y = 1 }
                                , { x = 0, y = 1 }
                                , { x = 0, y = 0 }
                                ]

                            moveMode =
                                getMoveMode Left lightPoints
                        in
                        Expect.equal Left moveMode
                    )
                ]
            , describe "showBox"
                [ test "LightOnの場合background-color redであること"
                    (\_ ->
                        let
                            dummyPoint =
                                { x = 0, y = 0 }

                            dummyBox =
                                { point = dummyPoint, lightMode = LightOn }

                            box =
                                showBox dummyBox
                        in
                        box
                            |> Query.fromHtml
                            |> Query.has [ Selector.style "background-color" "red" ]
                    )
                , test "LightOffの場合background-color whiteであること"
                    (\_ ->
                        let
                            dummyPoint =
                                { x = 0, y = 0 }

                            dummyBox =
                                { point = dummyPoint, lightMode = LightOff }

                            box =
                                showBox dummyBox
                        in
                        box
                            |> Query.fromHtml
                            |> Query.has [ Selector.style "background-color" "white" ]
                    )
                , test "class Boxを持っていること"
                    (\_ ->
                        let
                            dummyPoint =
                                { x = 0, y = 0 }

                            dummyBox =
                                { point = dummyPoint, lightMode = LightOn }

                            box =
                                showBox dummyBox
                        in
                        box
                            |> Query.fromHtml
                            |> Query.has [ Selector.class "box" ]
                    )
                ]
            ]
        ]
