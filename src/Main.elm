module Main exposing (..)

import Browser
import Browser.Events
import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Json.Decode as D exposing (Decoder)


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type LightMode
    = LightOn
    | LightOff


type alias Point =
    { x : Int
    , y : Int
    }


type alias Box =
    { point : Point
    , lightMode : LightMode
    }


type alias FieldSize =
    { rowSize : Int
    , columnSize : Int
    }


type MoveMode
    = Left
    | Right


type GameState
    = Playing
    | GameOver


type alias Model =
    { boxList : List Box
    , fieldSize : FieldSize
    , lightPoints : List Point
    , moveMode : MoveMode
    , stoppedLightPoints : List Point
    , gameState : GameState
    , beforeTime : Float
    }



-- UPDATE


type Msg
    = Blinking Float
    | KeyDown KeyType


type KeyType
    = Space
    | Other


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model.gameState of
        GameOver ->
            case msg of
                KeyDown keyType ->
                    case keyType of
                        Space ->
                            initialModel ()

                        Other ->
                            ( model
                            , Cmd.none
                            )

                Blinking _ ->
                    ( model
                    , Cmd.none
                    )

        Playing ->
            case msg of
                KeyDown keyType ->
                    case keyType of
                        Space ->
                            let
                                currentPoint =
                                    case model.lightPoints of
                                        [] ->
                                            { x = 0, y = 0 }

                                        headPoint :: rest ->
                                            headPoint

                                nextRow =
                                    { rowSize = currentPoint.x - 1, columnSize = 7 }

                                isStacked { x, y } =
                                    if List.isEmpty model.stoppedLightPoints then
                                        True

                                    else
                                        List.any (\p -> p == { x = x + 1, y = y }) model.stoppedLightPoints

                                stackedPoints =
                                    List.filter isStacked model.lightPoints
                            in
                            if List.isEmpty stackedPoints then
                                ( { model | gameState = GameOver }
                                , Cmd.none
                                )

                            else
                                ( { model
                                    | stoppedLightPoints = model.stoppedLightPoints ++ stackedPoints
                                    , lightPoints = setStartPoints nextRow 3
                                  }
                                , Cmd.none
                                )

                        Other ->
                            ( model, Cmd.none )

                Blinking deltaTime ->
                    let
                        concatLightPoints =
                            model.lightPoints ++ model.stoppedLightPoints

                        blinking box =
                            if List.member box.point concatLightPoints then
                                { box | lightMode = LightOn }

                            else
                                { box | lightMode = LightOff }

                        moveMode =
                            getMoveMode model.moveMode model.lightPoints

                        isUpdateTiming =
                            model.beforeTime + deltaTime > 500
                    in
                    if isUpdateTiming then
                        ( { model
                            | boxList = List.map blinking model.boxList
                            , lightPoints = moveLightPoints model.lightPoints model.moveMode
                            , moveMode = moveMode
                            , beforeTime = 0
                          }
                        , Cmd.none
                        )

                    else
                        ( { model | beforeTime = model.beforeTime + deltaTime }
                        , Cmd.none
                        )


getMoveMode : MoveMode -> List Point -> MoveMode
getMoveMode moveMode lightPoints =
    let
        headPoint =
            case lightPoints of
                [] ->
                    { x = 0, y = 0 }

                head :: rest ->
                    head

        length =
            List.length lightPoints

        tailPoint =
            case List.drop (length - 1) lightPoints of
                [] ->
                    { x = 0, y = 0 }

                tail :: rest ->
                    tail
    in
    case moveMode of
        Left ->
            if tailPoint.y == 1 then
                invertMoveMode moveMode

            else
                moveMode

        Right ->
            if headPoint.y == 5 then
                invertMoveMode moveMode

            else
                moveMode


invertMoveMode : MoveMode -> MoveMode
invertMoveMode moveMode =
    case moveMode of
        Left ->
            Right

        Right ->
            Left


moveLightPoints : List Point -> MoveMode -> List Point
moveLightPoints lightPoints moveMode =
    let
        movePoint { x, y } =
            { x = x, y = getOperator moveMode y 1 }
    in
    lightPoints
        |> List.map movePoint


getOperator : MoveMode -> number -> number -> number
getOperator moveMove =
    case moveMove of
        Left ->
            (-)

        Right ->
            (+)



-- VIEW


view : Model -> Html Msg
view model =
    let
        rowBaseSplitedBox =
            splitBox 7 model.boxList
    in
    div []
        [ div [] (List.map showBoxRow rowBaseSplitedBox)
        , div [] [ text <| showGameMessage model.gameState ]
        ]


showBox : Box -> Html Msg
showBox box =
    div
        [ Attr.class "box"
        , Attr.style "background-color" <| getLightColor box.lightMode
        ]
        []


showBoxRow : List Box -> Html Msg
showBoxRow boxList =
    div [ Attr.class "box-row" ]
        (List.map showBox boxList)


splitBox : Int -> List Box -> List (List Box)
splitBox n boxList =
    case List.take n boxList of
        [] ->
            []

        takedList ->
            takedList :: splitBox n (List.drop n boxList)


getLightColor : LightMode -> String
getLightColor lightMode =
    case lightMode of
        LightOn ->
            "red"

        LightOff ->
            "white"


showGameMessage : GameState -> String
showGameMessage gameState =
    case gameState of
        Playing ->
            "PRESS 'SPACE' TO STOP BOX"

        GameOver ->
            "GAME OVER! PRESS 'SPACE' TO TRY AGAIN"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameState of
        GameOver ->
            Browser.Events.onKeyDown (D.map KeyDown keyDecoder)

        Playing ->
            Sub.batch
                [ Browser.Events.onAnimationFrameDelta Blinking
                , Browser.Events.onKeyDown (D.map KeyDown keyDecoder)
                ]


keyDecoder : Decoder KeyType
keyDecoder =
    D.map toKey (D.field "key" D.string)


toKey : String -> KeyType
toKey key =
    case key of
        " " ->
            Space

        _ ->
            Other



-- INIT


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    let
        fieldSize =
            { rowSize = 10, columnSize = 7 }

        initPoint =
            { rowSize = 9, columnSize = 7 }

        pointList =
            makePointMatrix fieldSize
    in
    ( { boxList = List.map makeBox pointList
      , fieldSize = fieldSize
      , lightPoints = setStartPoints initPoint 3
      , moveMode = Left
      , stoppedLightPoints = []
      , gameState = Playing
      , beforeTime = 0
      }
    , Cmd.none
    )


setStartPoints : FieldSize -> Int -> List Point
setStartPoints fieldSize lightCount =
    let
        makeStartPoint y =
            { x = fieldSize.rowSize, y = y }

        headPoint =
            fieldSize.columnSize - lightCount

        tailPoint =
            fieldSize.columnSize - 1
    in
    List.range headPoint tailPoint
        |> List.map makeStartPoint


makePointMatrix : FieldSize -> List Point
makePointMatrix { rowSize, columnSize } =
    let
        rowNumList =
            List.range 0 (rowSize - 1)

        columnNumList =
            List.range 0 (columnSize - 1)
    in
    rowNumList
        |> List.map
            (\x ->
                columnNumList
                    |> List.map (\y -> { x = x, y = y })
            )
        |> List.concat


makeBox : Point -> Box
makeBox { x, y } =
    let
        point =
            { x = x
            , y = y
            }
    in
    { point = point, lightMode = LightOff }
