module DropDown exposing (Model, Msg(..), buildSelect, countries, init, main, strToOption, update, view)

import Browser
import Element as El exposing (column, html, layout, text)
import Html as Html exposing (Html, div, option, select, text)
import Html.Attributes as Attr exposing (value)
import Html.Events as Events exposing (on, onInput)
import Json.Decode as D


type alias Model =
    { selectedOption : String }


type Msg
    = SelectedOption String


countries =
    [ Model "Italy", Model "Spain", Model "Germany" ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        SelectedOption m ->
            { model | selectedOption = m }



-- VIEW -> wrap html/elm select inside an elm-ui Element


strToOption : String -> Html Msg
strToOption v =
    option [ Attr.value v ] [ Html.text v ]


buildSelect : List Model -> Html Msg
buildSelect options =
    select
        [ Events.on "change" <|
            D.map SelectedOption Events.targetValue
        ]
        (List.map strToOption <| List.map .selectedOption countries)


view : Model -> Html Msg
view model =
    El.layout [] <|
        El.column []
            [ El.html (buildSelect countries)
            , El.text model.selectedOption
            ]


init : Model
init =
    { selectedOption = "Italy" }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
