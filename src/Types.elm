module Types exposing (ProgramInput, Grid, Robot, Scent, Instructions, Instruction(..), Orientation(..))

type alias ProgramInput = ( Grid, List ( Robot, Instructions ) )

type alias Grid = ( Int, Int )

type alias Robot = ( Bool, Int, Int, Orientation )

type alias Scent = ( Int, Int, Orientation )

type alias Instructions = List Instruction

type Instruction = Left | Right | Forward
type Orientation = North | South | East | West
