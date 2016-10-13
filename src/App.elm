module App exposing (..)

import Html exposing (Html, div, textarea, p, text)
import Html.Attributes exposing (rows)
import Html.App
import Html.Events exposing (onInput)
import Task exposing (Task)
import Combine exposing (Parser, andThen, manyTill)
import Combine.Infix exposing (..)
import Combine.Char exposing (newline, space, oneOf)
import Combine.Num exposing (int)


main : Program Never
main =
    Html.App.program
        { init = ( initialModel, Cmd.none )
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
    = ChangeInput String
    | ChangeOutput String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp -> ( model, Cmd.none )

        ChangeInput s ->
            ( { model | input = s }, parseInput s )

        ChangeOutput s ->
            ( { model | output = s }, Cmd.none )


parseInput : String -> Cmd Msg
parseInput s =
    Task.perform (always NoOp) ChangeOutput (parseInputTask s)


parseInputTask : String -> Task Never String
parseInputTask s =
    Task.succeed (parse s)
    |> Task.map (\result ->
            case result of
                Ok res -> res
                Err msg -> Debug.log "parseInputTask error" msg)


-- VIEW


view : Model -> Html Msg
view ({input, output} as model) =
    div []
        [ textarea [ onInput ChangeInput, rows 20 ] [ text input ]
        , p [] [ text output ]
        , text (toString model)
        ]



-- PARSING


parse : String -> Result String String
parse s =
    case Combine.parse input s of
        (( Ok parsed, _ ) as res) ->
            let _ = Debug.log "parse result" res in
            Ok (toString parsed)

        ( Err msg, ctx ) ->
            Err <| "parse error: " ++ (toString msg) ++ ", " ++ (toString ctx)


input =
    coords
    `andThen` (\_ -> position)
    `andThen` (\_ -> instructions)


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
