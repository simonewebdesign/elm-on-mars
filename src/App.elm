module App exposing (..)

import Html exposing (Html, div, textarea, p, text)
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
    , output : String
    , robot  : Position
    , scents : List Position
    , grid   : ( Int, Int )
    }


initialModel : Model
initialModel =
    { input  = "5 3\n1 1 E\nRFRFRFRF\n\n3 2 N\nFRRFLLFFRRFLL\n\n0 3 W\nLLFFFLFLFL"
    , output = ""
    , robot  = initialPosition
    , scents = []
    , grid   = initialSize
    }

initialPosition = ( 0, 0, North )

initialSize = ( 0, 0 )

-- UPDATE


type Msg
    = NoOp
    | ChangeInput String
    | ChangeOutput String
    | SetGrid ( Int, Int )
    | SetPosition Position
    | ProcessInstruction Instruction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model, Cmd.none )

        ChangeInput s ->
            ( { model | input = s }, parse1stLine s )

        ChangeOutput s ->
            ( { model | output = s }, Cmd.none )

        SetGrid coords ->
            ( { model | grid = coords }, parse2ndLine model.input )

        SetPosition initialPos ->
            ( { model | robot = initialPos }, parse3rdLine model.input )

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
                        ( { model | robot = ( x, y, newOrientation ) }, parseNext )

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
                        ( { model | robot = ( x, y, newOrientation ) }, parseNext )

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
                        ( { model | robot = ( newX, newY, z ) }, parseNext )


parse1stLine : String -> Cmd Msg
parse1stLine str =
    Task.perform (always NoOp) SetGrid (parseCoordsTask str)


parseCoordsTask : String -> Task Never ( Int, Int )
parseCoordsTask str =
    Task.succeed (parseCoords str)
    |> Task.map (\result ->
            case result of
                Ok res -> res
                Err msg -> let _ = Debug.log "coords task failed" msg in initialSize)


parseCoords : String -> Result String ( Int, Int )
parseCoords str =
    case Combine.parse coords str of
        (( Ok parsed, {input} ) as res) ->
            let _ = Debug.log "grid size parsed" res in
            Ok parsed

        ( Err msg, ctx ) ->
            Err <| "parse error: " ++ (toString msg) ++ ", " ++ (toString ctx)


parse2ndLine : String -> Cmd Msg
parse2ndLine str =
    Task.perform (always NoOp) SetPosition (parsePositionTask str)


parsePositionTask : String -> Task Never Position
parsePositionTask str =
    Task.succeed (parsePosition str)
    |> Task.map (\result ->
            case result of
                Ok res -> res
                Err msg -> let _ = Debug.log "position task failed" msg in initialPosition)


parsePosition : String -> Result String Position
parsePosition str =
     case Combine.parse position str of
        ( Ok parsed, _ ) ->
            Ok parsed

        ( Err msg, ctx ) ->
            Err <| "parse error: " ++ (toString msg) ++ ", " ++ (toString ctx)


parse3rdLine : String -> Cmd Msg
parse3rdLine str =
    Task.perform (always NoOp) ProcessInstruction (parseInstructionTask str)


parseInstructionTask : String -> Task Never Instruction
parseInstructionTask str =
    Task.succeed (parseInstruction str)
    |> Task.map (\result ->
            case result of
                Ok res -> res
                Err msg -> let _ = Debug.log "instruction task failed" msg in Left)


parseInstruction : String -> Result String Instruction
parseInstruction str =
     case Combine.parse instruction str of
        ( Ok parsed, _ ) ->
            Ok parsed

        ( Err msg, ctx ) ->
            Err <| "parse error: " ++ (toString msg) ++ ", " ++ (toString ctx)


parseNext =
    Cmd.none


-- VIEW


view : Model -> Html Msg
view ({input, output} as model) =
    div []
        [ textarea [ onInput ChangeInput, rows 20 ] [ text input ]
        , p [] [ text output ]
        , text (toString model)
        ]



-- PARSING


--parse : String -> Result String String
--parse str =
--    case Combine.parse input str of
--        (( Ok parsed, _ ) as res) ->
--            let _ = Debug.log "parse result" res in
--            Ok (toString parsed)

--        ( Err msg, ctx ) ->
--            Err <| "parse error: " ++ (toString msg) ++ ", " ++ (toString ctx)


--input =
--    sequence [coords, position, instructions]

--input =
--    coords
--    `andThen` (\res ->
--        let
--            _ = Debug.log "coords" res
--            updateCoords = Task.perform (always NoOp) SetGrid (Task.succeed res)
--        in
--            position)
--    `andThen` (\res ->
--        let
--            _ = Debug.log "position" res

--        in
--            instructions)


        --Task.perform (always NoOp) MoveTo (moveRobotTask res))
    --`andThen` (\res -> newline)


--moveRobotTask : Position -> Task Never Position
--moveRobotTask pos =
--    Task.succeed pos `Task.andThen` instructions


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


instructions : Parser (List Instruction)
instructions =
    manyTill instruction newline


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
