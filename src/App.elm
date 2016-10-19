module App exposing (..)

import String exposing (left)
import Html exposing (Html, div, textarea, p, text, br)
import Html.Attributes exposing (rows)
import Html.App as Html
import Html.Events exposing (onInput)
import Parser exposing (parse)
import Types exposing (..)

main : Program Never
main =
    Html.beginnerProgram { model = initialModel, view = view, update = update }


-- MODEL

type alias Model =
    { input  : String
    , output : List String }

initialModel : Model
initialModel =
    { input  = "5 3\n1 1 E\nRFRFRFRF\n\n3 2 N\nFRRFLLFFRRFLL\n\n0 3 W\nLLFFFLFLFL\n\n"
    , output = [] }


-- VIEW

view : Model -> Html Msg
view {input, output} =
    div []
        [ textarea [ onInput Change, rows 20 ] [ text input ]
        , p [] <| List.intersperse (br[][]) (List.map text output)
        ]


-- UPDATE

type Msg = Change String

update : Msg -> Model -> Model
update (Change newInput) model =
    case parse newInput of
        Err msg -> { model | output = [msg] }
        Ok ( ( x, y ), pairs ) ->
            let
                newOutput : List String
                newOutput =
                    List.foldl processPair ( [], [] ) pairs
                    |> fst
                    |> List.map toStringRobot

                processPair : ( Robot, List Instruction ) -> ( List Robot, List Scent ) -> ( List Robot, List Scent )
                processPair ( robot, instructions ) ( robots, scents ) =
                    let
                        ( newRobot, newScents ) = List.foldl process ( robot, scents ) instructions
                    in
                        ( robots ++ [newRobot], scents ++ newScents )

                process : Instruction -> ( Robot, List Scent ) -> ( Robot, List Scent )
                process instruction ( (( isLost, a, b, c ) as robot), scents ) =
                    case instruction of
                        Forward ->
                            let
                                ( a', b' ) = forward a b c

                                inScent = List.member ( a, b, c ) scents

                                outOfBounds =
                                    a' > x || b' > y || a' < 0 || b' < 0
                            in
                                if inScent then
                                    ( robot, scents )

                                else if outOfBounds then
                                    ( ( True, a', b', c ), ( a, b, c ) :: scents )

                                else
                                    ( ( isLost, a', b', c ), scents )

                        _ ->
                            ( ( isLost, a, b, turn instruction c ), scents )
            in
                { model | input = newInput, output = newOutput }


forward : Int -> Int -> Orientation -> ( Int, Int )
forward a b c =
    let
        a' =
            case c of
                East -> a + 1
                West -> a - 1
                _ -> a
        b' =
            case c of
                North -> b + 1
                South -> b - 1
                _ -> b
    in
        ( a', b' )


turn : Instruction -> Orientation -> Orientation
turn instruction orientation =
    case instruction of
        Left ->
            case orientation of
                North -> West
                South -> East
                East -> North
                West -> South

        Right ->
            case orientation of
                North -> East
                South -> West
                East -> South
                West -> North

        Forward -> orientation


toStringRobot : Robot -> String
toStringRobot ( isLost, a, b, c ) =
    toString a ++ " " ++ toString b ++ " " ++ initial c
    ++ if isLost then " LOST" else ""


initial : a -> String
initial a =
    a |> toString |> left 1
