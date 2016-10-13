import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App
import Html.Events exposing (onInput)


main : Program Never
main =
    Html.App.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }


-- TYPES

type Orientation = North | South | East | West

type Instruction = Left | Right | Forward

--fromStringOrientation : String -> Orientation
--fromStringOrientation s =
--    case s of
--        "N" -> North
--        "S" -> South





-- MODEL


type alias Model =
    { input : String }


initialModel : Model
initialModel =
    { input = "ABC\nDEF" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change s -> { model | input = s }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "Hello, world!"
        , text (toString model)
        , textarea [ onInput Change ] []
        ]
