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
    , robot  = ( 0, 0, North )
    , scents = []
    , grid   = ( 0, 0 )
    }



-- UPDATE


type Msg
    = NoOp
    | ChangeInput String
    | ChangeOutput String
    | SetGrid ( Int, Int )
    --| MoveTo Position
    --| SetGrid ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model, Cmd.none )

        ChangeInput s ->
            ( { model | input = s }, parse1stLine s )

        ChangeOutput s ->
            ( { model | output = s }, Cmd.none )

        SetGrid coords ->
            ( { model | grid = coords }, Cmd.none )

        --MoveTo newPosition ->
        --    ( { model | robot = newPosition }, Cmd.none )


--parseInput : String -> Cmd Msg
--parseInput s =
--    Task.perform (always NoOp) ChangeOutput (parseInputTask s)


parse1stLine : String -> Cmd Msg
parse1stLine str =
    Task.perform (always NoOp) SetGrid (parseCoordsTask str)


parseCoordsTask : String -> Task Never ( Int, Int )
parseCoordsTask str =
    Task.succeed (parseCoords str)
    |> Task.map (\result ->
            case result of
                Ok res -> res
                Err msg -> let _ = Debug.log "task failed" msg in (0, 0))
                --Err msg -> )


parseCoords : String -> Result String ( Int, Int )
parseCoords str =
    case Combine.parse coords str of
        (( Ok parsed, _ ) as res) ->
            --let _ = Debug.log "parsed 1st line" res in
            Ok parsed

        ( Err msg, ctx ) ->
            Err <| "parse error: " ++ (toString msg) ++ ", " ++ (toString ctx)



--parseInputTask : String -> Task Never String
--parseInputTask s =
--    Task.succeed (parse s)
--    |> Task.map (\result ->
--            case result of
--                Ok res -> res
--                Err msg -> Debug.log "parseInputTask error" msg)


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
