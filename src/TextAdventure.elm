module TextAdventure exposing (Model, main)

import Browser
import Html exposing (div, form, input, p, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput, onSubmit)


type alias Model =
    { transcript : String
    , currentCommand : String
    }


type Msg
    = UpdateCommand String
    | SendCommand


initialModel : Model
initialModel =
    { transcript = "Welcome"
    , currentCommand = ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateCommand string ->
            { model | currentCommand = string }

        SendCommand ->
            { model | currentCommand = "", transcript = parseCommand model }


parseCommand model =
    if model.currentCommand == "n" then
        "You went north"

    else
        model.transcript


view : Model -> Html.Html Msg
view model =
    div []
        [ p [] [ text model.transcript ]
        , div []
            [ form [ onSubmit SendCommand ]
                [ input [ value model.currentCommand, onInput UpdateCommand ] []
                , input [ type_ "submit", value "Send" ] []
                ]
            ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
