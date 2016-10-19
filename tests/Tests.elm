module Tests exposing (..)

import Test exposing (..)
import Expect
import App
import Types exposing (..)


all : Test
all =
    describe "A Martian Test Suite"

        -- END-TO-END TESTS

        [ test "with no input" <|
            \() ->
                let newModel = App.update (App.Change "") App.initialModel
                in Expect.equal ["parse error: [\"expected an integer\"]"] newModel.output

        , test "end-to-end test #1" <|
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


        -- UNIT TESTS

        , test "turn left" <| \() -> Expect.equal West (App.turn Left North)
        , test "turn right" <| \() -> Expect.equal East (App.turn Right North)
        , test "go forward" <| \() -> Expect.equal North (App.turn Forward North)

        , test "toStringRobot" <|
            \() -> Expect.equal "25 12 N" (App.toStringRobot ( False, 25, 12, North ))
        , test "toStringRobot" <|
            \() -> Expect.equal "3 3 S LOST" (App.toStringRobot ( True, 3, 3, South ))

        , test "initial" <| \() -> Expect.equal "E" (App.initial East)
        , test "initial" <| \() -> Expect.equal "W" (App.initial West)
        ]
