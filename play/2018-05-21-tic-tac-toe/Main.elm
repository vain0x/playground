module Main exposing (Model, Msg, init, subscriptions, update, view)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Maybe exposing (Maybe)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Player
    = CirclePlayer
    | CrossPlayer


type Cell
    = EmptyCell
    | CircleCell
    | CrossCell


type alias Position =
    ( Int, Int )


type alias Board =
    Dict Position Cell


type alias Game =
    { activePlayer : Player
    , board : Board
    }


emptyBoard : Board
emptyBoard =
    let
        s =
            List.range 0 2
    in
    let
        assocs =
            List.concatMap (\y -> List.map (\x -> ( ( y, x ), EmptyCell )) s) s
    in
    Dict.fromList assocs


yourCell : Player -> Cell
yourCell player =
    case player of
        CirclePlayer ->
            CircleCell

        CrossPlayer ->
            CrossCell


opponent : Player -> Player
opponent player =
    case player of
        CirclePlayer ->
            CrossPlayer

        CrossPlayer ->
            CirclePlayer


put : Player -> Position -> Board -> ( Board, Player )
put player pos board =
    let
        next () =
            let
                cell =
                    yourCell player
            in
            let
                newBoard =
                    Dict.update pos (\_ -> Just cell) board
            in
            ( newBoard, opponent player )
    in
    let
        ignore () =
            ( board, player )
    in
    case Maybe.withDefault EmptyCell (Dict.get pos board) of
        EmptyCell ->
            next ()

        CircleCell ->
            ignore ()

        CrossCell ->
            ignore ()


type alias Model =
    { game : Game
    }


type Msg
    = Put Position


initGame : () -> Game
initGame () =
    { activePlayer = CirclePlayer
    , board = emptyBoard
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Put pos ->
            let
                ( board, player ) =
                    put model.game.activePlayer pos model.game.board
            in
            ( { game = { board = board, activePlayer = player } }, Cmd.none )


cellButton : Cell -> Position -> Html Msg
cellButton cell pos =
    let
        style =
            Attr.style
                [ ( "height", "40px" )
                , ( "width", "40px" )
                ]
    in
    let
        type_ =
            Attr.type_ "button"
    in
    case cell of
        EmptyCell ->
            button [ type_, style, Events.onClick (Put pos) ] [ text " " ]

        CircleCell ->
            button [ type_, style, Attr.disabled True ] [ text "○" ]

        CrossCell ->
            button [ type_, style, Attr.disabled True ] [ text "×" ]


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Tic Tac Toe" ]
        , text "Hello, world!"
        , form []
            (List.map
                (\( pos, cell ) -> cellButton cell pos)
                (Dict.toList model.game.board)
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( { game = initGame () }
    , Cmd.none
    )
