module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (id)
import Login
import Page.ListGames as ListGames
import Ports exposing (initGoogleAuth, receiveGoogleToken)
import RemoteData exposing (WebData)
import Route exposing (Route(..), parseUrl)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    }


type Msg
    = GamesPageMsg ListGames.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url
    | RecievedGoogleToken String
    | LoginMsg Login.Msg


type Page
    = NotFoundPage
    | GamesPage ListGames.Model


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url navKey =
    let
        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            }
    in
    initCurrentPage ( model, initGoogleAuth () )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.ListGames ->
                    let
                        ( pageModel, pageCmd ) =
                            ListGames.init
                    in
                    ( GamesPage pageModel, Cmd.map GamesPageMsg pageCmd )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


view : Model -> Document Msg
view model =
    { title = "Initiative"
    , body =
        [ div [ id "google-button-container" ] []
        , currentView model
        ]
    }


currentView : Model -> Html Msg
currentView model =
    case model.page of
        NotFoundPage ->
            notFoundView

        GamesPage pageModel ->
            ListGames.view pageModel |> Html.map GamesPageMsg


notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found!" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( RecievedGoogleToken token, _ ) ->
            ( model
            , Cmd.map LoginMsg <|
                Login.sendTokenToBackend token
            )

        ( GamesPageMsg subMsg, GamesPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    ListGames.update subMsg pageModel
            in
            ( { model | page = GamesPage updatedPageModel }
            , Cmd.map GamesPageMsg updatedCmd
            )

        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External url ->
                    ( model, Nav.load url )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    receiveGoogleToken RecievedGoogleToken
