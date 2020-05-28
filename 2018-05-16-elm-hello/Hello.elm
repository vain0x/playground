module Hello exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { count : Int
    }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { count = model.count - 1 }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Hello, Elm!" ]
        , text "Hello, world!"
        , text (toString model.count)
        , form []
            [ input
                [ Attr.type_ "text"
                , Attr.value (toString model.count)
                ]
                []
            , button [ Attr.type_ "button", Events.onClick Increment ] [ text "+" ]
            , button [ Attr.type_ "button", Events.onClick Decrement ] [ text "-" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( { count = 1 }
    , Cmd.none
    )
