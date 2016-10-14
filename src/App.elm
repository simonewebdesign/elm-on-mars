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

type alias Position = ( Int, Int, Orientation )

type Orientation = North | South | East | West

type Instruction = Left | Right | Forward


-- MODEL

type alias Model =
    { input  : String
    , output : List (Html Msg)
    , robot  : Position
    , scents : List Position
    , grid   : ( Int, Int )
    }


initialModel : Model
initialModel =
    { input  = "5 3\n1 1 E\nRFRFRFRF\n\n3 2 N\nFRRFLLFFRRFLL\n\n0 3 W\nLLFFFLFLFL"
    , output = initialOutput
    , robot  = initialPosition
    , scents = []
    , grid   = initialSize
    }

initialOutput = [text ""]

initialPosition = ( 0, 0, North )

initialSize = ( 0, 0 )


-- UPDATE

type alias Input = String

type Msg
    = NoOp
    | ChangeInput Input
    | ChangeOutput ( Input, Input )
    | SetGrid ( ( Int, Int ), Input )
    | SetPosition ( Position, Input )
    | ProcessInstructions ( (List Instruction), Input )
    | ProcessInstruction Instruction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "u" msg of
        NoOp -> ( model, Cmd.none )

        ChangeInput s ->
            ( { model | input = s, output = initialOutput }, parse1stLine s )

        ChangeOutput ( s, nextInput ) ->
            let _ = Debug.log "ChangeOutput" nextInput in
            ( { model | output = model.output ++ [text s, br [] []] }
            , if String.isEmpty nextInput then
                Cmd.none
              else
                parse2ndLine nextInput
            )

        SetGrid ( coords, nextInput ) ->
            ( { model | grid = coords }, parse2ndLine nextInput )

        SetPosition ( initialPos, nextInput ) ->
            ( { model | robot = initialPos }, parse3rdLine nextInput )

        ProcessInstructions (( list, nextInput ) as instructions) ->
            --let _ = Debug.log "ProcessInstructions" nextInput in
            case list of
                [] ->
                    let _ = Debug.log "DONE" nextInput in
                    update (ChangeOutput ( out model, nextInput )) model 
                    --( model, Cmd.none )
                    -- if .. then parse2ndLine else stop
                    --( model, setOutput model nextInput )

                instruction :: tail ->
                    let
                        ( processedInstruction, _ ) = update (ProcessInstruction instruction) model
                    in
                        update (ProcessInstructions ( tail, nextInput )) model

        ProcessInstruction instruction ->
            case instruction of
                Left ->
                    let
                        ( x, y, z ) = model.robot

                        newOrientation =
                            case z of
                                North -> West
                                South -> East
                                East -> North
                                West -> South
                    in
                        ( { model | robot = ( x, y, newOrientation ) }, Cmd.none )

                Right ->
                    let
                        ( x, y, z ) = model.robot

                        newOrientation =
                            case z of
                                North -> East
                                South -> West
                                East -> South
                                West -> North
                    in
                        ( { model | robot = ( x, y, newOrientation ) }, Cmd.none )

                Forward ->
                    let
                        ( x, y, z ) = model.robot

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
                    in
                        ( { model | robot = ( newX, newY, z ) }, Cmd.none )


parse1stLine : String -> Cmd Msg
parse1stLine str =
    Task.perform (always NoOp) SetGrid (parseCoordsTask str)


parseCoordsTask : String -> Task Never ( ( Int, Int ), String )
parseCoordsTask str =
    Task.succeed (parseCoords str)
    |> Task.map (\result ->
            case result of
                Ok res -> res
                Err msg -> let _ = Debug.log "coords task failed" msg in ( initialSize, "" ))


parseCoords : String -> Result String ( ( Int, Int ), String )
parseCoords str =
    case Combine.parse coords str of
        (( Ok parsed, {input} ) as res) ->
            --let _ = Debug.log "grid size parsed" res in
            Ok ( parsed, input )

        ( Err msg, ctx ) ->
            Err <| "parse error: " ++ toString msg -- ++ ", " ++ toString ctx


parse2ndLine : String -> Cmd Msg
parse2ndLine str =
    Task.perform (always NoOp) SetPosition (parsePositionTask str)


parsePositionTask : String -> Task Never ( Position, String )
parsePositionTask str =
    Task.succeed (parsePosition str)
    |> Task.map (\result ->
            case result of
                Ok res -> res
                Err msg -> let _ = Debug.log "position task failed" msg in ( initialPosition, "" ))


parsePosition : String -> Result String ( Position, String )
parsePosition str =
     case Combine.parse position str of
        ( Ok parsed, {input} ) ->
            Ok ( parsed, input )

        ( Err msg, ctx ) ->
            Err <| "parse error: " ++ toString msg -- ++ ", " ++ toString ctx


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
            Err <| "parse error: " ++ toString msg -- ++ ", " ++ toString ctx


setOutput : Model -> Input -> Cmd Msg
setOutput model input =
    Task.perform (always NoOp) ChangeOutput (newOutput model input)


newOutput : Model -> Input -> Task Never ( String, Input )
newOutput model input =
    let
        ( x, y, z ) = model.robot
        _ = Debug.log "newOutput robot" model.robot
    in
        (Task.succeed <| out model)
        |> Task.map (\str -> ( str, input ))

{-| This function figures out how to print the answer based on the model. -}
out : Model -> String
out model =
    let
        ( x, y, z ) = model.robot
    in
        toString x ++ " " ++ toString y ++ " "
        ++ String.fromChar (toCharOrientation z)


--parseNext : String -> Cmd Msg
--parseNext str =
--    let _ = Debug.log "parseNext" str in
--    Task.perform (always NoOp) ChangeOutput (Task.succeed ("str", "str"))


-- VIEW

view : Model -> Html Msg
view ({input, output} as model) =
    div []
        [ textarea [ onInput ChangeInput, rows 20 ] [ text input ]
        , p [] output
        , text (toString model)
        ]


-- PARSING

coords : Parser ( Int, Int )
coords =
    (,) <$> int <* space <*> int <* newline


position : Parser Position
position =
    (,,) <$> int <* space <*> int <* space <*> orientation <* newline


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
