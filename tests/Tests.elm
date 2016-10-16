module Tests exposing (..)

import Test exposing (..)
import Expect
import App

input = "5 3\n1 1 E\nRFRFRFRF\n\n3 2 N\nFRRFLLFFRRFLL\n\n0 3 W\nLLFFFLFLFL\n\n"
output = ["1 1 E", "3 3 N LOST", "2 3 S"]

all : Test
all =
    describe "A Martian Test Suite"
        [
          --test "input output" <|
          --  \() ->
          --      let
          --          ( newModel, _ ) =
          --             App.update
          --                 (App.ChangeInput input)
          --                 newModel
          --      in
          --          Expect.equal output newModel.output
        ]
