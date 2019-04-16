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
    , offsets : List Int
    , message : String
    }


nth : Int -> List a -> Maybe a
nth index xs =
    xs |> List.drop index |> List.head


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


closeSuggestion : Int -> Model -> Model
closeSuggestion index model =
    let
        ( rawOffset, newModel ) =
            popOffset model

        newIndex =
            -- FIXME: Too bad use of random number
            rawOffset |> remainderBy (List.length model.suggestions)

        newSuggestionId =
            newModel.suggestions
                |> nth newIndex
                |> Maybe.map (\suggestion -> suggestion.id)

        -- Replace the closed one with new one if possible.
        newActiveSuggestionIds =
            newModel.activeSuggestionIds
                |> List.indexedMap
                    (\i suggestionId ->
                        if i == index then
                            newSuggestionId

                        else
                            Just suggestionId
                    )
                |> List.filterMap (\x -> x)
    in
    { newModel | activeSuggestionIds = newActiveSuggestionIds }


popOffset : Model -> ( Int, Model )
popOffset model =
    case model.offsets of
        [] ->
            -- FIXME: refresh offsets
            ( 0, model )

        offset :: offsets ->
            ( offset, { model | offsets = offsets } )


randomOffsets : (List Int -> Msg) -> Cmd Msg
randomOffsets mkMsg =
    Random.int 0 500
        |> Random.list 100
        |> Random.generate mkMsg


fetchUsers : Int -> Cmd Msg
fetchUsers offset =
    Http.get
        { url = "https://api.github.com/users?since=" ++ String.fromInt offset
        , expect = Http.expectJson GotSuggestionList userDecoder
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { suggestions = []
      , activeSuggestionIds = []
      , offsets = []
      , message = "Loading..."
      }
    , randomOffsets (\offsets -> GotOffsets offsets Refresh)
    )


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
    = GenerateOffsets Msg
    | GotOffsets (List Int) Msg
    | Refresh
    | GotSuggestionList (Result Http.Error (List Suggestion))
    | Close Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateOffsets nextMsg ->
            ( model, randomOffsets (\offsets -> GotOffsets offsets nextMsg) )

        GotOffsets offsets nextMsg ->
            let
                nextModel =
                    { model | offsets = offsets }
            in
            update nextMsg nextModel

        Refresh ->
            let
                ( offset, nextModel ) =
                    popOffset model
            in
            ( { nextModel
                | message = "Fetching..."
              }
            , fetchUsers offset
            )

        GotSuggestionList result ->
            case result of
                Ok suggestions ->
                    ( { model
                        | suggestions = suggestions
                        , activeSuggestionIds = initActiveSuggestionIds suggestions
                        , message = "Completed"
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model | message = "Failed" }, Cmd.none )

        Close index ->
            ( closeSuggestion index model, Cmd.none )



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

        viewSuggestion index suggestion =
            [ img
                (src suggestion.avatarUrl :: avatarStyles)
                []
            , a
                (href "#" :: usernameStyles)
                [ text suggestion.username ]
            , a
                [ style "user-selection" "none"
                , href "#"
                , onClick (Close index)
                ]
                [ text "x" ]
            ]
                |> li listItemStyles
    in
    (activeSuggestions model |> List.indexedMap viewSuggestion) |> ul listStyles


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
