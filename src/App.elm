module App exposing (..)

import Html exposing (Html, div, textarea, p, text, br)
import Html.Attributes exposing (rows)
import Html.App as Html
import Html.Events exposing (onInput)

import Combine exposing (Parser, manyTill, sepBy1)
import Combine.Infix exposing (..)
import Combine.Char exposing (newline, space, oneOf, eol)
import Combine.Num exposing (int)

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


-- UPDATE

type Msg = Change String

update : Msg -> Model -> Model
update msg model =
    case msg of
        Change str ->
            parse str
            |> handleInput model str


handleInput : Model -> String -> (Result String ProgramInput) -> Model
handleInput model newInput result =
    case result of
        Err msg -> { model | output = [msg] }
        Ok ( ( x, y ), pairs ) ->
            let
                newOutput : List String
                newOutput =
                    List.map toStringRobot newRobots

                newRobots : List Robot
                newRobots =
                    List.foldl processPair ( [], [] ) pairs
                    |> fst

                processPair : ( Robot, List Instruction ) -> ( List Robot, List Scent ) -> ( List Robot, List Scent )
                processPair ( robot, instructions ) ( robotsAcc, scents ) =
                    let
                        ( newRobot, newScents ) = List.foldl process ( robot, scents ) instructions
                    in
                        ( robotsAcc ++ [newRobot], scents ++ newScents )

                process : Instruction -> ( Robot, List Scent ) -> ( Robot, List Scent )
                process instruction ( (( isLost, a, b, c ) as robot), scents ) =
                    case instruction of
                        Forward ->
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


parse : String -> Result String ProgramInput
parse str =
    case Combine.parse programInput str of
        ( Ok parsed, _ ) -> Ok parsed
        ( Err msg, _ ) -> Err <| "parse error: " ++ toString msg


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
toStringRobot ( isLost, x, y, z ) =
    toString x ++ " " ++ toString y ++ " " ++ toStringOrientation z
    ++ if isLost then " LOST" else ""


-- VIEW

view : Model -> Html Msg
view {input, output} =
    div []
        [ textarea [ onInput Change, rows 20 ] [ text input ]
        , p [] <| List.intersperse (br[][]) (List.map text output)
        ]


-- PARSING

programInput : Parser ProgramInput
programInput =
    (,) <$> grid <* newline
        <*> sepBy1 newline pair


pair : Parser ( Robot, Instructions )
pair =
    (,) <$> position <* newline
        <*> instructions


grid : Parser Grid
grid =
    (,) <$> int <* space <*> int


position : Parser Robot
position =
    (,,,) False <$> int <* space <*> int <* space <*> orientation


orientation : Parser Orientation
orientation =
    toOrientation <$> oneOf ['N', 'S', 'E', 'W']

toOrientation : Char -> Orientation
toOrientation c =
    case c of
        'N' -> North
        'S' -> South
        'E' -> East
        'W' -> West
        _ -> Debug.crash "wat"

toStringOrientation : Orientation -> String
toStringOrientation i =
    case i of
        North -> "N"
        South -> "S"
        East -> "E"
        West -> "W"


instructions : Parser Instructions
instructions =
    manyTill instruction (newline <|> eol)

instruction : Parser Instruction
instruction =
    toInstruction <$> oneOf ['L', 'R', 'F']

toInstruction : Char -> Instruction
toInstruction c =
    case c of
        'L' -> Left
        'R' -> Right
        'F' -> Forward
        _ -> Debug.crash "wat"
