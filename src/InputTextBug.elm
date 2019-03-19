module InputTextBug exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Element exposing (column, el, fill, height, layout)
import Element.Input as Input
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { currentMessage : String }


initialModel : Model
initialModel =
    { currentMessage = "" }


type Msg
    = SendMessage
    | MessageUpdate String


update : Msg -> Model -> Model
update msg model =
    case msg of
        MessageUpdate message ->
            { model | currentMessage = message }

        SendMessage ->
            { model | currentMessage = "" }


view : Model -> Html Msg
view model =
    div []
        [ viewHtml model
        , viewUi model
        , button [ onClick SendMessage ] [ text "Send" ]
        ]


viewHtml model =
    div []
        [ div []
            [ Html.input [ value model.currentMessage, onInput MessageUpdate ] [] ]
        , div [ Html.Attributes.id "t1" ]
            [ Html.textarea [ value model.currentMessage, onInput MessageUpdate ] []
            ]
        ]


viewUi model =
    layout [ height fill ] <|
        column []
            [ Input.text []
                { onChange = MessageUpdate
                , text = model.currentMessage
                , placeholder = Nothing
                , label = Input.labelHidden "Skriv"
                }
            , el [] <| Element.text "If you type below, it sticks in t2."
            , Input.multiline
                [ Element.htmlAttribute (Html.Attributes.id "t2")
                ]
                { onChange = MessageUpdate
                , text = model.currentMessage
                , placeholder = Nothing
                , label = Input.labelHidden "Skriv"
                , spellcheck = True
                }
            ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
