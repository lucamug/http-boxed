module Http.Boxed exposing
    ( Boxed(..)
    , Error(..)
    , body
    , fromResponseExpectingJson
    , fromResponseExpectingString
    , headers
    , status
    , toString
    , unboxResponse
    )

import Dict
import Http
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type Error body
    = BadUrl String
    | Timeout
    | NetworkError
    | BadPayload String (Http.Response String)
    | BadStatus (Http.Response String)
    | BadStatusGoodPayload (Http.Response body)


type Boxed body
    = NotRequested
    | Fetching
    | Success (Http.Response body)
    | Error (Error body)


type alias HttpStatus =
    { code : Int
    , message : String
    }


body : Boxed body -> Maybe body
body boxed =
    case boxed of
        Success response ->
            Just response.body

        Error error ->
            case error of
                BadStatusGoodPayload response ->
                    Just response.body

                _ ->
                    Nothing

        _ ->
            Nothing


status : Boxed a -> Maybe Int
status boxed =
    case boxed of
        Success response ->
            Just response.status.code

        Error error ->
            case error of
                BadPayload _ response ->
                    Just response.status.code

                BadStatus response ->
                    Just response.status.code

                BadStatusGoodPayload response ->
                    Just response.status.code

                _ ->
                    Nothing

        _ ->
            Nothing


headers : Boxed a -> Maybe (Dict.Dict String String)
headers boxed =
    case boxed of
        Success response ->
            Just response.headers

        Error error ->
            case error of
                BadPayload _ response ->
                    Just response.headers

                BadStatus response ->
                    Just response.headers

                BadStatusGoodPayload response ->
                    Just response.headers

                _ ->
                    Nothing

        _ ->
            Nothing


convertHttpErrorinBoxedError : Http.Error -> Error body
convertHttpErrorinBoxedError error =
    case error of
        Http.BadUrl string ->
            BadUrl string

        Http.Timeout ->
            Timeout

        Http.NetworkError ->
            NetworkError

        Http.BadStatus response ->
            BadStatus response

        Http.BadPayload string response ->
            BadPayload string response


toString : Boxed body -> String
toString boxed =
    case boxed of
        NotRequested ->
            "NotRequested"

        Fetching ->
            "Fetching"

        Success response ->
            "Success\n\n" ++ Json.Encode.encode 4 (encodeResponse response)

        Error error ->
            "Error-"
                ++ (case error of
                        BadUrl url ->
                            "BadUrl " ++ url

                        Timeout ->
                            "Timeout"

                        NetworkError ->
                            "NetworkError"

                        BadStatus response ->
                            "BadStatus\n\n" ++ Json.Encode.encode 4 (encodeResponse response)

                        BadPayload description response ->
                            "BadPayload\n\n" ++ Json.Encode.encode 4 (encodeResponse response)

                        BadStatusGoodPayload response ->
                            "BadStatusGoodPayload\n\n" ++ Json.Encode.encode 4 (encodeResponse response)
                   )


decodeResponse : Json.Decode.Decoder (Http.Response String)
decodeResponse =
    Json.Decode.succeed Http.Response
        |> Json.Decode.Pipeline.required "url" Json.Decode.string
        |> Json.Decode.Pipeline.required "status"
            (Json.Decode.succeed HttpStatus
                |> Json.Decode.Pipeline.required "code" Json.Decode.int
                |> Json.Decode.Pipeline.required "message" Json.Decode.string
            )
        |> Json.Decode.Pipeline.required "headers" (Json.Decode.dict Json.Decode.string)
        |> Json.Decode.Pipeline.required "body" Json.Decode.string


encodeResponse : Http.Response a -> Json.Encode.Value
encodeResponse record =
    Json.Encode.object
        [ ( "url", Json.Encode.string <| record.url )
        , ( "status"
          , Json.Encode.object
                [ ( "code", Json.Encode.int <| record.status.code )
                , ( "message", Json.Encode.string <| record.status.message )
                ]
          )
        , ( "headers", Json.Encode.dict identity Json.Encode.string record.headers )
        , ( "body", Json.Encode.string <| "N/A" )
        ]


unboxResponse : Http.Response String -> Result String (Http.Response String)
unboxResponse outerResponse =
    {- This take a response from http, it check if it is a nested type (use for testing).
       If it is nested, it returns the inner version, otherwise it return the response
       as it is
    -}
    case Json.Decode.decodeString decodeResponse outerResponse.body of
        Ok innerResponse ->
            Ok innerResponse

        Err _ ->
            Ok outerResponse


codeIsSuccess : Int -> Bool
codeIsSuccess code =
    200 >= code && code < 300


fromResponseExpectingJson : Result Http.Error (Http.Response String) -> Json.Decode.Decoder body -> Boxed body
fromResponseExpectingJson response decoderBody =
    case response of
        Err err ->
            Error <| convertHttpErrorinBoxedError err

        Ok res ->
            -- Dilemma: should we check status code first or parse the json first?
            -- if the status code is not success, should we keep parsing the body?
            case Json.Decode.decodeString decoderBody res.body of
                Err err ->
                    Error <| BadPayload (Json.Decode.errorToString err) res

                Ok b ->
                    let
                        resNew =
                            { url = res.url
                            , status = res.status
                            , headers = res.headers
                            , body = b
                            }
                    in
                    if codeIsSuccess res.status.code then
                        Success resNew

                    else
                        Error <| BadStatusGoodPayload resNew


fromResponseExpectingString : Result Http.Error (Http.Response String) -> Boxed String
fromResponseExpectingString response =
    case response of
        Err err ->
            Error <| convertHttpErrorinBoxedError err

        Ok res ->
            if codeIsSuccess res.status.code then
                Success res

            else
                Error <| BadStatusGoodPayload res
