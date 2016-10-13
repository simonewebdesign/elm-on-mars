import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Html.Events exposing (onInput)


main : Program Never
main =
    Html.App.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
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
    , output : String }


initialModel : Model
initialModel =
    { input  = "5 3\n1 1 E\nRFRFRFRF\n3 2 N\nFRRFLLFFRRFLL\n0 3 W\nLLFFFLFLFL"
    , output = "" }



-- UPDATE


type Msg
    = ChangeInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeInput s ->
            ( { model | input = s }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view ({input, output} as model) =
    div []
        [ textarea [ onInput ChangeInput ] [ text input ]
        , p [] [ text output ]
        , text (toString model)
        ]
