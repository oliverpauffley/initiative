port module Ports exposing (initGoogleAuth, receiveGoogleToken)


port initGoogleAuth : () -> Cmd msg


port receiveGoogleToken : (String -> msg) -> Sub msg
