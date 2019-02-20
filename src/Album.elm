module Album exposing (Msg(..), main)

import Browser
import Element exposing (Element, alignRight, centerX, column, el, fill, height, image, layout, maximum, none, paddingXY, paragraph, row, shrink, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select
import Html exposing (Html)
import Http
import Task


type Msg
    = RequestFile
    | FileLoaded File
    | GotPreview String
    | StartUpload (Maybe File)
    | GotProgress Http.Progress
    | Uploaded (Result Http.Error ())


type alias Model =
    { previewUrl : Maybe String
    , file : Maybe File
    , fileSize : Int
    , uploading : Float
    , errors : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing Nothing 0 0 ""
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Msg " msg of
        RequestFile ->
            ( model, File.Select.file [ "image/jpeg" ] FileLoaded )

        FileLoaded file ->
            ( { model | file = Just file, fileSize = File.size file }, Task.perform GotPreview (File.toUrl file) )

        GotPreview url ->
            ( { model | previewUrl = Just url }, Cmd.none )

        StartUpload maybeFile ->
            case maybeFile of
                Nothing ->
                    ( model, Cmd.none )

                Just file ->
                    ( { model | uploading = 0 }
                    , Http.request
                        { method = "POST"
                        , url = "http://localhost:5000/photos/store/" ++ "b794eee4-11d8-4c86-a6dc-002949d1e8b7"
                        , headers = []
                        , body =
                            Http.multipartBody
                                [ Http.filePart "file" file
                                ]
                        , expect = Http.expectWhatever Uploaded
                        , timeout = Nothing
                        , tracker = Just "upload"
                        }
                    )

        GotProgress progress ->
            case progress of
                Http.Sending p ->
                    ( { model | uploading = Http.fractionSent p }, Cmd.none )

                Http.Receiving _ ->
                    ( model, Cmd.none )

        Uploaded result ->
            case result of
                Ok _ ->
                    ( { model | previewUrl = Nothing }, Cmd.none )

                Err error ->
                    ( { model | errors = httpErrorString error }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Http.track "upload" GotProgress


view : Model -> Html Msg
view model =
    layout [ height fill ] <|
        column [ height fill, width fill ]
            [ row [ Border.width 1, width fill ]
                [ content model
                ]
            , row []
                [ paragraph []
                    [ text model.errors
                    , text <| "Upload: " ++ String.fromFloat model.uploading
                    , text <| "Size: " ++ String.fromInt model.fileSize
                    ]
                ]
            ]


content : Model -> Element Msg
content model =
    column
        [ width fill
        , height fill
        ]
        [ previewImage model
        , Input.button buttonAttributes
            { onPress = Just RequestFile
            , label = el [ centerX ] <| text "Choose"
            }
        ]


previewImage : Model -> Element Msg
previewImage model =
    column [ width shrink, Border.width 1 ]
        [ case model.previewUrl of
            Nothing ->
                none

            Just url ->
                image [ width (fill |> maximum 300) ]
                    { src = url
                    , description = "preview of image"
                    }
        , Input.button buttonAttributes
            { onPress = Just (StartUpload model.file)
            , label = el [ centerX ] <| text "Send to server"
            }
        ]


buttonAttributes =
    [ Border.rounded 3
    , Border.widthEach { bottom = 3, top = 0, right = 0, left = 0 }
    , Font.bold
    , paddingXY 20 6
    , alignRight
    ]


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadBody message ->
            message

        Http.BadStatus statusCode ->
            "Server error: " ++ String.fromInt statusCode

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        Http.NetworkError ->
            "Network error "

        Http.Timeout ->
            "Request timeout "


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
