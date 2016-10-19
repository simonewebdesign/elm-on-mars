module Tests exposing (..)

import Test exposing (..)
import Expect
import App


all : Test
all =
    describe "A Martian Test Suite"
        [ test "end-to-end test #1" <|
            \() ->
                let newModel = App.update (App.Change input) App.initialModel
                    input = "5 3\n1 1 E\nRFRFRFRF\n\n3 2 N\nFRRFLLFFRRFLL\n\n0 3 W\nLLFFFLFLFL\n"
                    output = ["1 1 E", "3 3 N LOST", "2 3 S"]
                in Expect.equal output newModel.output

        , test "end-to-end test #2" <|
            \() ->
                let newModel = App.update (App.Change input) App.initialModel
                    input = "15 25\n12 6 E\nFFRLLFLFFFFFFFFFFFFFFF\n"
                    output = ["-1 7 W LOST"]
                in Expect.equal output newModel.output
        ]
