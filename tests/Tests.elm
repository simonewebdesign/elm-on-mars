module Tests exposing (..)

import Test exposing (..)
import Expect
import App


all : Test
all =
    describe "A Martian Test Suite"
        [ test "input output" <|
            \() ->
                let
                    ( newModel, _ ) =
                       App.update
                           (App.ChangeInput "5 3\n1 1 E\nRFRFRFRF\n\n3 2 N\nFRRFLLFFRRFLL\n\n0 3 W\nLLFFFLFLFL")
                           App.initialModel
                in
                    Expect.equal "1 1 E\n3 3 N LOST\n2 3 S" newModel.output
        ]
