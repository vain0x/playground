module Main exposing (Model, Msg(..), Suggestion, SuggestionId, activeSuggestions, fetchUsers, findSuggestionById, init, initActiveSuggestionIds, main, subscriptions, update, userDecoder, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as D exposing (Decoder)
import Random



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
    , username : String
    , avatarUrl : String
    }


type alias Model =
    { suggestions : List Suggestion
    , activeSuggestionIds : List SuggestionId
    , message : String
    }


nth : Int -> List a -> Maybe a
nth index xs =
    xs |> List.drop index |> List.head


listReplace : a -> a -> List a -> List a
listReplace src dest xs =
    xs
        |> List.map
            (\x ->
                if x == src then
                    dest

                else
                    x
            )


findSuggestionById : SuggestionId -> Model -> Maybe Suggestion
findSuggestionById suggestionId model =
    model.suggestions
        |> List.filter (\suggestion -> suggestion.id == suggestionId)
        |> List.head


initActiveSuggestionIds : List Suggestion -> List SuggestionId
initActiveSuggestionIds suggestions =
    suggestions
        |> List.take 3
        |> List.map (\suggestion -> suggestion.id)


activeSuggestions : Model -> List Suggestion
activeSuggestions model =
    model.activeSuggestionIds
        |> List.filterMap (\suggestionId -> findSuggestionById suggestionId model)


closeSuggestion : SuggestionId -> Int -> Model -> Model
closeSuggestion oldSuggestionId newIndex model =
    let
        newSuggestionId =
            model.suggestions
                |> nth newIndex
                |> Maybe.map (\suggestion -> suggestion.id)
                |> Maybe.withDefault oldSuggestionId

        newActiveSuggestionIds =
            model.activeSuggestionIds
                |> listReplace oldSuggestionId newSuggestionId
    in
    { model | activeSuggestionIds = newActiveSuggestionIds }


randomOffset : (Int -> Msg) -> Cmd Msg
randomOffset mkMsg =
    Random.int 0 500
        |> Random.generate mkMsg


randomIndex : List a -> (Int -> Msg) -> Cmd Msg
randomIndex xs mkMsg =
    Random.int 0 (List.length xs - 1)
        |> Random.generate mkMsg


fetchUsers : Int -> Cmd Msg
fetchUsers offset =
    Http.get
        { url = "https://api.github.com/users?since=" ++ String.fromInt offset
        , expect = Http.expectJson GotSuggestionList userDecoder
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            { suggestions = []
            , activeSuggestionIds = []
            , message = "Loading..."
            }
    in
    update Refresh model


userDecoder : Decoder (List Suggestion)
userDecoder =
    let
        user =
            D.map3 Suggestion
                (D.field "id" D.int)
                (D.field "login" D.string)
                (D.field "avatar_url" D.string)
    in
    D.list user



-- UPDATE


type Msg
    = Refresh
    | FetchSuggestionList Int
    | GotSuggestionList (Result Http.Error (List Suggestion))
    | Close SuggestionId
    | CloseEnd SuggestionId Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Refresh ->
            ( { model | message = "Loading..." }
            , randomOffset FetchSuggestionList
            )

        FetchSuggestionList offset ->
            ( model, fetchUsers offset )

        GotSuggestionList result ->
            case result of
                Ok suggestions ->
                    ( { model
                        | suggestions = suggestions
                        , activeSuggestionIds = initActiveSuggestionIds suggestions
                        , message = ""
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | message = "Something wrong" }, Cmd.none )

        Close oldSuggestionId ->
            ( model
            , randomIndex model.suggestions (CloseEnd oldSuggestionId)
            )

        CloseEnd oldSuggestionId index ->
            ( closeSuggestion oldSuggestionId index model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewSuggestions : Model -> Html Msg
viewSuggestions model =
    let
        listStyles =
            [ style "list-style" "none"
            , style "padding" "5px"
            , style "display" "flex"
            , style "flex-flow" "column nowrap"
            , style "width" "max-content"
            ]

        listItemStyles =
            [ style "padding" "5px"
            , style "display" "flex"
            , style "flex-flow" "row nowrap"
            , style "align-items" "center"
            ]

        avatarStyles =
            [ style "width" "40px"
            , style "height" "40px"
            , style "border-radius" "20px"
            ]

        usernameStyles =
            [ style "flex" "1"
            , style "margin" "0 5px"
            ]

        viewSuggestion suggestion =
            [ img
                (src suggestion.avatarUrl :: avatarStyles)
                []
            , a
                (href "#" :: usernameStyles)
                [ text suggestion.username ]
            , a
                [ style "user-selection" "none"
                , href "#"
                , onClick (Close suggestion.id)
                ]
                [ text "x" ]
            ]
                |> li listItemStyles
    in
    activeSuggestions model
        |> List.map viewSuggestion
        |> ul listStyles


viewHeader : Html Msg
viewHeader =
    header
        [ style "backgroundColor" "#ececec"
        , style "padding" "5px"
        ]
        [ h2
            [ style "display" "inline-block"
            ]
            [ text "Who to follow" ]
        , viewRefreshButton
        ]


viewRefreshButton : Html Msg
viewRefreshButton =
    a
        [ style "margin-left" "10px"
        , style "font-size" "80%"
        , href "#"
        , onClick Refresh
        ]
        [ text "Refresh"
        ]


view : Model -> Html Msg
view model =
    article
        [ style "padding" "10px"
        ]
        [ viewHeader
        , main_ [ style "border" "2px solid #ECECEC" ]
            [ viewSuggestions model
            , text model.message
            ]
        ]
