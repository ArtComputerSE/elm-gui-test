module Album exposing (Msg(..), main)

import Browser
import Element exposing (Element, alignRight, centerX, column, el, fill, height, image, layout, maximum, none, paddingXY, row, text, width)
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select
import Html exposing (Html)
import Task


type Msg
    = GotFile File
    | RequestFile
    | FileLoaded String


type alias Model =
    { fileContent : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Msg " msg of
        RequestFile ->
            ( model, File.Select.file [ "image/jpeg" ] GotFile )

        GotFile file ->
            ( model, Task.perform FileLoaded (File.toUrl file) )

        FileLoaded s ->
            ( { model | fileContent = Just s }, Cmd.none )


view : Model -> Html Msg
view model =
    layout [ height fill ] <|
        column [ height fill, width fill ]
            [ row [ Border.width 1, width fill ]
                [ content model.fileContent
                ]
            ]


content : Maybe String -> Element Msg
content fileContent =
    column
        [ width fill
        , height fill
        ]
        [ case fileContent of
            Nothing ->
                none

            Just url ->
                image [ width (fill |> maximum 300) ]
                    { src = url
                    , description = "preview"
                    }
        , Input.button buttonAttributes
            { onPress = Just RequestFile
            , label = el [ centerX ] <| text "Choose"
            }
        ]


buttonAttributes =
    [ Border.rounded 3
    , Border.widthEach { bottom = 3, top = 0, right = 0, left = 0 }
    , Font.bold
    , paddingXY 20 6
    , alignRight
    ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
