module Page.ListGames exposing (Model, Msg, init, update, view)

import Html exposing (..)


type alias Model =
    {}


initModel : Model
initModel =
    {}


type alias Msg =
    {}


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


view : Model -> Html Msg
view _ =
    div [] [ text "view games" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
