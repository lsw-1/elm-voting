module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode.Pipeline as JsonPipe exposing (..)
import Http exposing (..)
import Json.Encode as Encode exposing (encode, object, string)
import Json.Decode as Decode exposing (..)
import RemoteData exposing (WebData)


---- MODEL ----


type Status
    = Failed
    | NotLoggedIn
    | Success


type alias Speaker =
    { name : String
    , description : String
    }


type alias Model =
    { name : String
    , password : String
    , token : WebData String
    , status : Status
    , speakers : WebData (List Speaker)
    }


init : ( Model, Cmd Msg )
init =
    ( { name = ""
      , password = ""
      , token = RemoteData.NotAsked
      , status = NotLoggedIn
      , speakers = RemoteData.NotAsked
      }
    , Cmd.none
    )



---- UPDATE ----


type alias LoginData =
    { name : String
    , password : String
    }


type Msg
    = UpdateName String
    | UpdatePassword String
    | SendLogin LoginData
    | LoginRequest (Result Http.Error String)
    | OnFetchSpeakers (WebData (List Speaker))
    | OnLogin (WebData String)
    | FetchSpeakers


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateName s ->
            ( { model | name = s }, Cmd.none )

        UpdatePassword s ->
            ( { model | password = s }, Cmd.none )

        SendLogin o ->
            ( model, loginRequest o )

        LoginRequest (Ok t) ->
            ( { model | status = Success }, Cmd.none )

        LoginRequest (Err _) ->
            ( { model
                | status = Failed
              }
            , Cmd.none
            )

        FetchSpeakers ->
            ( model, fetchSpeakers )

        OnFetchSpeakers res ->
            ( { model | speakers = res }, Cmd.none )

        OnLogin res ->
            ( { model | token = res }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ h1 [] [ text "Speakers Corner" ]
            ]
        , div []
            [ h4 []
                [ text "Login form" ]
            , Html.form [ onSubmit <| SendLogin { name = model.name, password = model.password } ]
                [ label
                    []
                    [ text "Name: " ]
                , input [ type_ "text", onInput UpdateName ] []
                , label [] [ text "Password: " ]
                , input [ type_ "password", onInput UpdatePassword ] []
                , input [ type_ "submit" ] [ text "Submit" ]
                ]
            , div []
                [ button [ onClick FetchSpeakers ] [ text "click here" ]
                , maybeList model.speakers
                ]
            ]
        ]


maybeList : WebData (List Speaker) -> Html Msg
maybeList res =
    case res of
        RemoteData.NotAsked ->
            text "none"

        RemoteData.Loading ->
            text "Loading"

        RemoteData.Success speakers ->
            text (speakers |> toString)

        RemoteData.Failure error ->
            text (toString error)



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }



---- HTTP ----


baseUrl : String
baseUrl =
    "https://dev-voting-api-bs18.herokuapp.com/api"



-- LOGIN


loginRequest : LoginData -> Cmd Msg
loginRequest data =
    let
        url =
            baseUrl ++ "/auth/login"
    in
        Http.post url (Http.jsonBody <| encodeLoginData data) loginDecoder
            |> RemoteData.sendRequest
            |> Cmd.map OnLogin 


encodeLoginData : LoginData -> Value
encodeLoginData d =
    Encode.object
        [ ( "name", Encode.string d.name )
        , ( "password", Encode.string d.password )
        ]


loginDecoder : Decode.Decoder String
loginDecoder =
    Decode.at [ "token" ] Decode.string



-- SPEAKERS


fetchSpeakers : Cmd Msg
fetchSpeakers =
    Http.get (baseUrl ++ "/speakers") speakersDecoder
        |> RemoteData.sendRequest
        |> Cmd.map OnFetchSpeakers


speakersDecoder : Decode.Decoder (List Speaker)
speakersDecoder =
    Decode.list speakerDecoder


speakerDecoder : Decode.Decoder Speaker
speakerDecoder =
    JsonPipe.decode Speaker
        |> JsonPipe.required "name" Decode.string
        |> JsonPipe.required "description" Decode.string
