module Login exposing (Msg, SessionToken, loginDecode, sendTokenToBackend, tokenEncoder)

import Http
import Json.Decode as Decode
import Json.Encode as Encode


backendUrl : String
backendUrl =
    "http://localhost:8080"


type Msg
    = LoggedIn (Result Http.Error SessionToken)


sendTokenToBackend : String -> Cmd Msg
sendTokenToBackend token =
    Http.post
        { url = backendUrl ++ "login"
        , body = Http.jsonBody (tokenEncoder token)
        , expect = Http.expectJson LoggedIn loginDecode
        }


tokenEncoder : String -> Encode.Value
tokenEncoder tok =
    Encode.object
        [ ( "token", Encode.string tok ) ]


type SessionToken
    = SessionToken String


loginDecode : Decode.Decoder SessionToken
loginDecode =
    Decode.map SessionToken Decode.string
