module Main exposing (Model, Msg, init, subscriptions, update)

import Procon exposing (..)


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { lines : List String
    }


type Msg
    = NoOp
    | ReadLine String



-- INIT


init : () -> ( Model, Cmd Msg )
init () =
    ( { lines = [] }, Cmd.none )



-- UPDATE


listWords : List String -> List String
listWords lines =
    lines
        |> List.map (\line -> line |> String.trimRight |> String.split " ")
        |> List.concat


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ReadLine line ->
            let
                lines =
                    line :: model.lines

                words =
                    lines |> List.reverse |> listWords

                cmd =
                    case words of
                        a :: b :: c :: s :: [] ->
                            case ( String.toInt a, String.toInt b, String.toInt c ) of
                                ( Just intA, Just intB, Just intC ) ->
                                    let
                                        sum =
                                            intA + intB + intC
                                    in
                                    writeLine ( String.fromInt sum ++ " " ++ s, True )

                                _ ->
                                    -- error
                                    exit 1

                        _ ->
                            -- continue to read inputs
                            Cmd.none
            in
            ( { model | lines = lines }, cmd )


didReadLine : String -> Msg
didReadLine value =
    value |> ReadLine



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    readLine didReadLine
