module HtmlDropDown exposing (Model, Msg(..), init, main)

import Browser
import Html
import Html.Attributes exposing (selected)
import Html.Events


type alias Model =
    { selection : String
    }


type Msg
    = NoOp String


init : Model
init =
    { selection = ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp s ->
            { model | selection = s }


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.text "Hello"
        , Html.select
            [ Html.Events.onInput NoOp ]
            [ toOption model "uno"
            , toOption model "dos"
            , toOption model "tres"
            , toOption model "cuatro"
            ]
        ]


toOption model str =
    Html.option [ selected (model.selection == str) ] [ Html.text str ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
