module Parser exposing (parse)

import Combine exposing (Parser, manyTill, sepBy1)
import Combine.Infix exposing (..)
import Combine.Char exposing (newline, space, oneOf, eol)
import Combine.Num exposing (int)
import Types exposing (..)

parse : String -> Result String ProgramInput
parse str =
    case Combine.parse programInput str of
        ( Ok parsed, _ ) -> Ok parsed
        ( Err msg, _ ) -> Err <| "parse error: " ++ toString msg


programInput : Parser ProgramInput
programInput =
    (,) <$> grid <* newline
        <*> sepBy1 newline pair


pair : Parser ( Robot, Instructions )
pair =
    (,) <$> position <* newline
        <*> instructions


grid : Parser Grid
grid =
    (,) <$> int <* space <*> int


position : Parser Robot
position =
    (,,,) False <$> int <* space <*> int <* space <*> orientation


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


instructions : Parser Instructions
instructions =
    manyTill instruction (newline <|> eol)

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
