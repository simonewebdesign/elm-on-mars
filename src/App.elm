module App exposing (..)

import Html exposing (Html, div, textarea, p, text)
--import Html.Attributes exposing (..)
import Html.App
import Html.Events exposing (onInput)
import Task exposing (Task)
import Combine exposing (..)
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

--fromStringOrientation : String -> Orientation
--fromStringOrientation s =
--    case s of
--        "N" -> North
--        "S" -> South





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
    Task.succeed (parser s)
    |> Task.map (\result ->
            case result of
                Ok res ->
                    toString res
                Err msg ->
                    Debug.log "parseInputTask error" msg
        )

-- VIEW


view : Model -> Html Msg
view ({input, output} as model) =
    div []
        [ textarea [ onInput ChangeInput ] [ text input ]
        , p [] [ text output ]
        , text (toString model)
        ]



-- PARSING

parser : String -> Result String Int
parser s =
    case parse int s of
        (( Ok str, _ ) as res) ->
            let _ = Debug.log "result" res in
            Ok str

        ( Err msg, ctx ) ->
            Err <| "parse error: " ++ (toString msg) ++ ", " ++ (toString ctx)


--coords : Parser ( Int, Int )
--coords =

