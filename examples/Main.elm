module Main exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Events
import Http
import Http.Boxed
import HttpBuilder
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Url
import Url.Builder



{-
   ███    ███  ██████  ██████  ███████ ██
   ████  ████ ██    ██ ██   ██ ██      ██
   ██ ████ ██ ██    ██ ██   ██ █████   ██
   ██  ██  ██ ██    ██ ██   ██ ██      ██
   ██      ██  ██████  ██████  ███████ ███████
-}


type alias Model =
    { httpBoxedJson : Http.Boxed.Boxed BodyResponse
    , httpBoxedText : Http.Boxed.Boxed String
    , httpUrl : String
    }



{-
   ██ ███    ██ ██ ████████
   ██ ████   ██ ██    ██
   ██ ██ ██  ██ ██    ██
   ██ ██  ██ ██ ██    ██
   ██ ██   ████ ██    ██
-}


init : () -> ( Model, Cmd Msg )
init _ =
    ( { httpBoxedJson = Http.Boxed.NotRequested
      , httpBoxedText = Http.Boxed.NotRequested
      , httpUrl = ""
      }
    , Cmd.none
    )



{-
   ███    ███ ███████  ██████
   ████  ████ ██      ██
   ██ ████ ██ ███████ ██   ███
   ██  ██  ██      ██ ██    ██
   ██      ██ ███████  ██████
-}


type Msg
    = MorePlease String
    | NewGifExpectingJson (Result Http.Error (Http.Response String))
    | NewGifExpectingText (Result Http.Error (Http.Response String))
    | MsgFetching
    | MsgNotRequested



{-
   ██    ██ ██████  ██████   █████  ████████ ███████
   ██    ██ ██   ██ ██   ██ ██   ██    ██    ██
   ██    ██ ██████  ██   ██ ███████    ██    █████
   ██    ██ ██      ██   ██ ██   ██    ██    ██
    ██████  ██      ██████  ██   ██    ██    ███████
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease url ->
            ( { model
                | httpUrl = url
                , httpBoxedJson = Http.Boxed.Fetching
                , httpBoxedText = Http.Boxed.Fetching
              }
            , Cmd.batch
                [ getRandomGif url True
                , getRandomGif url False
                ]
            )

        MsgFetching ->
            ( { model
                | httpUrl = ""
                , httpBoxedJson = Http.Boxed.Fetching
                , httpBoxedText = Http.Boxed.Fetching
              }
            , Cmd.none
            )

        MsgNotRequested ->
            ( { model
                | httpUrl = ""
                , httpBoxedJson = Http.Boxed.NotRequested
                , httpBoxedText = Http.Boxed.NotRequested
              }
            , Cmd.none
            )

        NewGifExpectingJson result ->
            let
                httpBoxedJson =
                    Http.Boxed.fromResponseExpectingJson result decoderBodyResponse
            in
            ( { model
                | httpBoxedJson = httpBoxedJson
              }
            , Cmd.none
            )

        NewGifExpectingText result ->
            let
                httpBoxedText =
                    Http.Boxed.fromResponseExpectingString result
            in
            ( { model
                | httpBoxedText = httpBoxedText
              }
            , Cmd.none
            )



{-
   ███████ ██    ██ ██████  ███████  ██████ ██████  ██ ██████  ████████ ██  ██████  ███    ██ ███████
   ██      ██    ██ ██   ██ ██      ██      ██   ██ ██ ██   ██    ██    ██ ██    ██ ████   ██ ██
   ███████ ██    ██ ██████  ███████ ██      ██████  ██ ██████     ██    ██ ██    ██ ██ ██  ██ ███████
        ██ ██    ██ ██   ██      ██ ██      ██   ██ ██ ██         ██    ██ ██    ██ ██  ██ ██      ██
   ███████  ██████  ██████  ███████  ██████ ██   ██ ██ ██         ██    ██  ██████  ██   ████ ███████
-}


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



{-
   ██    ██ ██ ███████ ██     ██
   ██    ██ ██ ██      ██     ██
   ██    ██ ██ █████   ██  █  ██
    ██  ██  ██ ██      ██ ███ ██
     ████   ██ ███████  ███ ███
-}


view : Model -> Html.Html Msg
view model =
    let
        commonAttr =
            [ Html.Attributes.style "width" "100px"
            , Html.Attributes.style "display" "inline-block"
            , Html.Attributes.style "padding" "8px"
            ]
    in
    Html.div []
        [ Html.h1 [] [ Html.text "Http.Boxed" ]
        , Html.p [] [ Html.text "Mocking api module. This module allow to mock APIs without installing external tool or servers. It is enough to load resposnes saved as json files." ]
        , Html.p []
            [ Html.text "Source code: "
            , Html.a
                [ Html.Attributes.href "https://github.com/lucamug/http-boxed" ]
                [ Html.text "https://github.com/lucamug/http-boxed" ]
            ]
        , Html.p [] [ Html.text "Click the buttons below for usage examples." ]
        , Html.div []
            [ Html.div
                commonAttr
                [ Html.text "Unboxed" ]
            , viewButton model "api-tests/unboxed.json" "200"
            , viewButton model "api-tests/unboxedEmptyJson.json" "\"{}\" 200"
            , viewButton model "api-tests/unboxedEmptyString.json" "\"\" 200"
            ]
        , Html.div []
            [ Html.div
                commonAttr
                [ Html.text "Boxed" ]
            , viewButton model "api-tests/boxed200.json" "200"
            , viewButton model "api-tests/boxed200EmptyJson.json" "\"{}\" 200"
            , viewButton model "api-tests/boxed200EmptyString.json" "\"\" 200"
            , viewButton model "api-tests/boxed409.json" "409"
            , viewButton model "api-tests/boxed409EmptyJson.json" "\"{}\" 409"
            , viewButton model "api-tests/boxed409EmptyString.json" "\"\" 409"
            , viewButton model "api-tests/boxed409Headers.json" "409 Headers"
            ]
        , Html.div []
            [ Html.div
                commonAttr
                [ Html.text "Others" ]
            , viewButton model (toGiphyUrl "cat") "Real API call"
            , viewButton model "api-tests/unexistingFile.json" "Unexisting File"
            , viewButton model "https://unexistingDomain.com" "Unexisting Domain"
            , viewButton model "https://" "Bad Url"
            , viewButton2 model Http.Boxed.NotRequested MsgNotRequested "Not Requested"
            , viewButton2 model Http.Boxed.Fetching MsgFetching "Fetching"
            ]
        , Html.h2 [] [ Html.text <| "model.httpUrl" ]
        , Html.p [] [ Html.text <| model.httpUrl ]
        , Html.iframe
            [ Html.Attributes.style "border" "1px solid #ddd"
            , Html.Attributes.style "width" "100%"
            , Html.Attributes.style "height" "160px"
            , Html.Attributes.src model.httpUrl
            ]
            []
        , Html.h2 [] [ Html.text <| "Http.Boxed.toString" ]
        , Html.pre [ Html.Attributes.style "padding" "10px" ] [ Html.text <| "Json: " ++ Http.Boxed.toString model.httpBoxedJson ]
        , Html.pre [ Html.Attributes.style "padding" "10px" ] [ Html.text <| "Text: " ++ Http.Boxed.toString model.httpBoxedText ]

        --
        , Html.h2 [] [ Html.text <| "Http.Boxed.status" ]
        , Html.div [ Html.Attributes.style "padding" "10px" ] [ Html.text <| "Json: " ++ (Debug.toString <| Http.Boxed.status model.httpBoxedJson) ]
        , Html.div [ Html.Attributes.style "padding" "10px" ] [ Html.text <| "Text: " ++ (Debug.toString <| Http.Boxed.status model.httpBoxedText) ]

        --
        , Html.h2 [] [ Html.text <| "Http.Boxed.headers" ]
        , Html.div [ Html.Attributes.style "padding" "10px" ] [ Html.text <| "Json: " ++ (Debug.toString <| Http.Boxed.headers model.httpBoxedJson) ]
        , Html.div [ Html.Attributes.style "padding" "10px" ] [ Html.text <| "Text: " ++ (Debug.toString <| Http.Boxed.headers model.httpBoxedText) ]

        --
        , Html.h2 [] [ Html.text <| "Http.Boxed.body" ]
        , Html.div [ Html.Attributes.style "padding" "10px" ] [ Html.text <| "Json: " ++ (Debug.toString <| Http.Boxed.body model.httpBoxedJson) ]
        , Html.div [ Html.Attributes.style "padding" "10px" ] [ Html.text <| "Text: " ++ (Debug.toString <| Http.Boxed.body model.httpBoxedText) ]

        --
        , Html.h2 [] [ Html.text <| "Debug.toString" ]
        , Html.div [ Html.Attributes.style "padding" "10px" ] [ Html.text <| "Json: " ++ Debug.toString model.httpBoxedJson ]
        , Html.div [ Html.Attributes.style "padding" "10px" ] [ Html.text <| "Text: " ++ Debug.toString model.httpBoxedText ]
        ]


buttonAttributes =
    [ Html.Attributes.style "padding" "5px 10px"
    , Html.Attributes.style "margin" "0 10px"
    , Html.Attributes.style "font-size" "0.9em"
    ]


viewButton model httpUrl description =
    Html.button
        (buttonAttributes
            ++ (if httpUrl == model.httpUrl then
                    [ Html.Attributes.style "background-color" "#aaaaff" ]

                else
                    []
               )
            ++ [ Html.Events.onClick <| MorePlease httpUrl
               ]
        )
        [ Html.text description ]


viewButton2 model type_ msg description =
    Html.button
        (buttonAttributes
            ++ (if model.httpBoxedJson == type_ then
                    [ Html.Attributes.style "background-color" "#aaaaff" ]

                else
                    []
               )
            ++ [ Html.Events.onClick <| msg ]
        )
        [ Html.text description ]



{-
   ██   ██ ████████ ████████ ██████
   ██   ██    ██       ██    ██   ██
   ███████    ██       ██    ██████
   ██   ██    ██       ██    ██
   ██   ██    ██       ██    ██
-}


getRandomGif : String -> Bool -> Cmd Msg
getRandomGif url expectingJson =
    url
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectStringResponse Http.Boxed.unboxResponse)
        |> HttpBuilder.send
            (if expectingJson then
                NewGifExpectingJson

             else
                NewGifExpectingText
            )


toGiphyUrl : String -> String
toGiphyUrl topic =
    Url.Builder.crossOrigin "https://api.giphy.com"
        [ "v1", "gifs", "random" ]
        [ Url.Builder.string "api_key" "dc6zaTOxFJmzC"
        , Url.Builder.string "tag" topic
        ]



{-
   ██████  ███████  ██████  ██████  ██████  ███████ ██████  ███████
   ██   ██ ██      ██      ██    ██ ██   ██ ██      ██   ██ ██
   ██   ██ █████   ██      ██    ██ ██   ██ █████   ██████  ███████
   ██   ██ ██      ██      ██    ██ ██   ██ ██      ██   ██      ██
   ██████  ███████  ██████  ██████  ██████  ███████ ██   ██ ███████
-}
-- from https://becoming-functional.com/handling-real-world-json-data-in-elm-c1816c7b3620


type alias Imageurl =
    { image_url : String
    }


type alias Error =
    { key : String
    , value : String
    }


type BodyResponse
    = Success Imageurl
    | Errors (List Error)


type alias ImageurlValidResponse =
    { data : Imageurl
    }


type alias ErrorObject =
    { errors : List Error
    }


imageurlDecoder : Json.Decode.Decoder Imageurl
imageurlDecoder =
    Json.Decode.succeed Imageurl
        |> Json.Decode.Pipeline.required "image_url" Json.Decode.string


userResponseDecoder : Json.Decode.Decoder ImageurlValidResponse
userResponseDecoder =
    Json.Decode.succeed ImageurlValidResponse
        |> Json.Decode.Pipeline.required "data" imageurlDecoder


errorDecoder : Json.Decode.Decoder Error
errorDecoder =
    Json.Decode.succeed Error
        |> Json.Decode.Pipeline.required "key" Json.Decode.string
        |> Json.Decode.Pipeline.required "value" Json.Decode.string


errorListDecoder : Json.Decode.Decoder (List Error)
errorListDecoder =
    Json.Decode.list errorDecoder


errorResponseDecoder : Json.Decode.Decoder ErrorObject
errorResponseDecoder =
    Json.Decode.succeed ErrorObject
        |> Json.Decode.Pipeline.required "errors" errorListDecoder


successResponse : Json.Decode.Decoder BodyResponse
successResponse =
    Json.Decode.map
        (\response -> Success response.data)
        userResponseDecoder


errorResponse : Json.Decode.Decoder BodyResponse
errorResponse =
    Json.Decode.map
        (\response -> Errors response.errors)
        errorResponseDecoder


decoderBodyResponse : Json.Decode.Decoder BodyResponse
decoderBodyResponse =
    Json.Decode.oneOf
        [ successResponse
        , errorResponse
        ]


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
