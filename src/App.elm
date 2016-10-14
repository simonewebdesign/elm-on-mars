module App exposing (..)

import String
import Html exposing (Html, div, textarea, p, text, br)
import Html.Attributes exposing (rows)
import Html.App
import Html.Events exposing (onInput)
import Task exposing (Task)
import Combine exposing (Parser, manyTill)
import Combine.Infix exposing (..)
import Combine.Char exposing (newline, space, oneOf)
import Combine.Num exposing (int)


main : Program Never
main =
    Html.App.program
        { init = ( initialModel, parse1stLine initialModel.input )
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


-- TYPES

type alias Grid = ( Int, Int )

type alias Robot = ( Lost, Int, Int, Orientation )

type alias Scent = ( Int, Int, Orientation )

type alias Lost = Bool

type Orientation = North | South | East | West

type Instruction = Left | Right | Forward


-- MODEL

type alias Model =
    { input  : String
    , output : List (Html Msg)
    , robot  : Robot
    , scents : List Scent
    , grid   : Grid
    }


initialModel : Model
initialModel =
    { input  = "5 3\n1 1 E\nRFRFRFRF\n\n3 2 N\nFRRFLLFFRRFLL\n\n0 3 W\nLLFFFLFLFL\n\n"
    , output = initialOutput
    , robot  = initialRobot
    , scents = []
    , grid   = initialSize
    }

initialOutput = [text ""]

initialRobot = ( False, 0, 0, North )

initialSize = ( 0, 0 )


-- UPDATE

type alias Input = String

type Msg
    = NoOp
    | ChangeInput Input
    | ChangeOutput ( Input, Input )
    | SetGrid ( Grid, Input )
    | SetRobot ( Robot, Input )
    | ProcessInstructions ( (List Instruction), Input )
    | ProcessInstruction Instruction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "m" msg of
        NoOp -> ( model, Cmd.none )

        ChangeInput s ->
            ( { initialModel | input = s }, parse1stLine s )

        ChangeOutput ( s, nextInput ) ->
            ( { model | output = model.output ++ [text s, br [] []] }
            , if String.isEmpty nextInput then
                Cmd.none
              else
                parse2ndLine nextInput
            )

        SetGrid ( coords, nextInput ) ->
            ( { model | grid = coords }, parse2ndLine nextInput )

        SetRobot ( initialPos, nextInput ) ->
            ( { model | robot = initialPos }, parse3rdLine nextInput )

        ProcessInstructions (( list, nextInput ) as instructions) ->
            case list of
                [] ->
                    update (ChangeOutput ( out model, nextInput )) model

                instruction :: tail ->
                    let
                        ( newModel, _ ) = update (ProcessInstruction instruction) model
                    in
                        update (ProcessInstructions ( tail, nextInput )) newModel

        ProcessInstruction instruction ->
            let
                ( isLost, x, y, z ) = model.robot
            in
                case instruction of
                    Left ->
                        let
                            newOrientation =
                                case z of
                                    North -> West
                                    South -> East
                                    East -> North
                                    West -> South
                        in
                            ( { model | robot = ( isLost, x, y, newOrientation ) }, Cmd.none )

                    Right ->
                        let
                            newOrientation =
                                case z of
                                    North -> East
                                    South -> West
                                    East -> South
                                    West -> North
                        in
                            ( { model | robot = ( isLost, x, y, newOrientation ) }, Cmd.none )

                    Forward ->
                        let
                            newX =
                                case z of
                                    East -> x + 1
                                    West -> x - 1
                                    _ -> x

                            newY =
                                case z of
                                    North -> y + 1
                                    South -> y - 1
                                    _ -> y

                            inScent = List.member ( x, y, z ) model.scents

                            outOfBounds =
                                newX > fst model.grid || newY > snd model.grid ||
                                newX < 0 || newY < 0
                        in
                            if inScent then
                                ( model, Cmd.none )
                            else if outOfBounds then
                                ( { model
                                    | robot = ( True, newX, newY, z )
                                    , scents = ( x, y, z ) :: model.scents }
                                , Cmd.none
                                )
                            else
                                ( { model | robot = ( isLost, newX, newY, z ) }, Cmd.none )


parse1stLine : String -> Cmd Msg
parse1stLine str =
    Task.perform (always NoOp) SetGrid (parseCoordsTask str)


parseCoordsTask : String -> Task Never ( Grid, String )
parseCoordsTask str =
    Task.succeed (parseCoords str)
    |> Task.map (\result ->
            case result of
                Ok res -> res
                Err msg -> let _ = Debug.log "coords task failed" msg in ( initialSize, "" ))


parseCoords : String -> Result String ( Grid, String )
parseCoords str =
    case Combine.parse coords str of
        (( Ok parsed, {input} ) as res) ->
            Ok ( parsed, input )

        ( Err msg, ctx ) ->
            Err <| "parse error: " ++ toString msg


parse2ndLine : String -> Cmd Msg
parse2ndLine str =
    Task.perform (always NoOp) SetRobot (parseRobotTask str)


parseRobotTask : String -> Task Never ( Robot, String )
parseRobotTask str =
    Task.succeed (parseRobot str)
    |> Task.map (\result ->
            case result of
                Ok res -> res
                Err msg -> let _ = Debug.log "position task failed" msg in ( initialRobot, "" ))


parseRobot : String -> Result String ( Robot, String )
parseRobot str =
     case Combine.parse position str of
        ( Ok parsed, {input} ) ->
            Ok ( parsed, input )

        ( Err msg, ctx ) ->
            Err <| "parse error: " ++ toString msg


parse3rdLine : String -> Cmd Msg
parse3rdLine str =
    Task.perform (always NoOp) ProcessInstructions (parseInstructionsTask str)


parseInstructionsTask : String -> Task Never ( (List Instruction), Input )
parseInstructionsTask str =
    Task.succeed (parseInstructions str)
    |> Task.map (\result ->
            case result of
                Ok res -> res
                Err msg -> let _ = Debug.log "instructions task failed" msg in ( [], "" ))


parseInstructions : String -> Result String ( (List Instruction), Input )
parseInstructions str =
     case Combine.parse instructions str of
        ( Ok parsed, {input} ) ->
            Ok ( parsed, input )

        ( Err msg, ctx ) ->
            Err <| "parse error: " ++ toString msg


setOutput : Model -> Input -> Cmd Msg
setOutput model input =
    Task.perform (always NoOp) ChangeOutput (newOutput model input)


newOutput : Model -> Input -> Task Never ( String, Input )
newOutput model input =
    let
        ( _, x, y, z ) = model.robot
    in
        (Task.succeed <| out model)
        |> Task.map (\str -> ( str, input ))

{-| This function figures out how to print the answer based on the model. -}
out : Model -> String
out model =
    let
        ( isLost, x, y, z ) = model.robot
        ( gridX, gridY ) = model.grid
    in
        toString x ++ " " ++ toString y ++ " "
        ++ String.fromChar (toCharOrientation z)
        ++ if isLost then " LOST" else ""


-- VIEW

view : Model -> Html Msg
view ({input, output} as model) =
    div []
        [ textarea [ onInput ChangeInput, rows 20 ] [ text input ]
        , p [] output
        ]


-- PARSING

coords : Parser Grid
coords =
    (,) <$> int <* space <*> int <* newline


position : Parser Robot
position =
    (,,,) False <$> int <* space <*> int <* space <*> orientation <* newline


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


toCharOrientation : Orientation -> Char
toCharOrientation i =
    case i of
        North -> 'N'
        South -> 'S'
        East -> 'E'
        West -> 'W'


instructions : Parser (List Instruction)
instructions =
    manyTill instruction newline <* newline


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
