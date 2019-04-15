module Main exposing (Model, Msg(..), Suggestion, SuggestionId, SuggestionList, init, main, subscriptions, suggestionListEmpty, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as D exposing (Decoder, field, string)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias SuggestionId =
    Int


type alias Suggestion =
    { id : SuggestionId
    , name : String
    }


type alias SuggestionList =
    { suggestions : List Suggestion
    , activeSuggestionIds : List SuggestionId
    }


suggestionListEmpty : SuggestionList
suggestionListEmpty =
    { suggestions = []
    , activeSuggestionIds = []
    }


type alias Model =
    { suggestionList : SuggestionList
    , message : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { suggestionList = suggestionListEmpty
      , message = "Loading..."
      }
    , Http.get
        { url = "https://api.github.com/users"
        , expect = Http.expectJson GotSuggestionList userDecoder
        }
    )


userDecoder : Decoder (List Suggestion)
userDecoder =
    let
        user =
            D.map2 Suggestion
                (D.field "id" D.int)
                (D.field "login" D.string)
    in
    D.list user



-- UPDATE


type Msg
    = GotSuggestionList (Result Http.Error (List Suggestion))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSuggestionList result ->
            case result of
                Ok suggestions ->
                    let
                        suggestionList =
                            model.suggestionList
                    in
                    ( { model
                        | suggestionList =
                            { suggestionList
                                | suggestions = suggestions
                            }
                        , message = "Completed"
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | message = "Failed" }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    section []
        [ text model.message
        , ul []
            (model.suggestionList.suggestions
                |> List.map
                    (\suggestion ->
                        li []
                            [ div [ class "username" ]
                                [ text suggestion.name ]
                            ]
                    )
            )
        ]
