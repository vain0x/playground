port module Procon exposing (exit, readLine, writeLine)


port exit : Int -> Cmd msg


port writeLine : ( String, Bool ) -> Cmd msg


port readLine : (String -> msg) -> Sub msg
