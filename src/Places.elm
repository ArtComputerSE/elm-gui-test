module Places exposing (Model, Msg(..), main)

import AllColors exposing (themeColor0, themeColor1)
import Browser
import Element exposing (Attribute, Element, alignRight, centerX, clip, column, el, fill, height, inFront, layout, maximum, moveLeft, moveUp, paddingXY, px, row, spacingXY, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type alias Model =
    { places : List String
    , showPlacesSelection : Bool
    }


type Msg
    = AddButtonPressed
    | DoneSelecting
    | TogglePlace String
    | NoOp Bool


init =
    Model [] False


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddButtonPressed ->
            { model | showPlacesSelection = True }

        DoneSelecting ->
            { model | showPlacesSelection = False }

        TogglePlace place ->
            let
                newList =
                    if List.member place model.places then
                        List.filter (\p -> p /= place) model.places

                    else
                        place :: model.places
            in
            { model | places = newList }

        NoOp _ ->
            model


view : Model -> Html Msg
view model =
    layout [ height fill ] <|
        column
            [ width fill
            , centerX
            , paddingXY 30 30
            , Background.color themeColor0
            ]
            [ viewPlacesControl model ]


viewPlacesControl : Model -> Element Msg
viewPlacesControl model =
    row
        ([ width fill
         , spacingXY 5 5
         ]
            ++ modal model
        )
        [ el [] <| text "Orter: "
        , viewPlaces model
        , addButton
        ]


modal : Model -> List (Attribute Msg)
modal model =
    if model.showPlacesSelection then
        selectionDialog model.places

    else
        []


viewPlaces : Model -> Element Msg
viewPlaces model =
    row
        [ Border.width 1
        , width fill
        , paddingXY 5 5
        , spacingXY 5 5
        ]
        (List.map viewPlace model.places)


viewPlace place =
    row
        [ Border.width 1
        , Border.rounded 25
        , paddingXY 5 3
        ]
        [ el [ paddingXY 3 3 ] <| text place
        , Input.button
            []
            { onPress = Just (TogglePlace place)
            , label =
                el
                    [ Border.width 1
                    , Border.rounded 25
                    , clip
                    , paddingXY 3 3
                    ]
                <|
                    text "X"
            }
        ]


addButton : Element Msg
addButton =
    Input.button
        []
        { onPress = Just AddButtonPressed
        , label =
            el
                [ Border.width 2
                , paddingXY 3 3
                ]
            <|
                text "+"
        }


selectionDialog : List String -> List (Attribute Msg)
selectionDialog selectedPlaces =
    [ inFront <|
        el
            [ Background.color themeColor1
            , alignRight
            , moveUp 5
            , moveLeft 5
            , paddingXY 3 3
            , Font.size 12
            , Border.rounded 3
            , width (fill |> maximum 700)
            ]
        <|
            placesSelector selectedPlaces
    ]


placesSelector : List String -> Element Msg
placesSelector selectedPlaces =
    column [ width fill ]
        [ placesSelectionGrid selectedPlaces
        , Input.button [ alignRight ]
            { onPress = Just DoneSelecting
            , label =
                el
                    [ Border.width 2
                    , paddingXY 3 3
                    ]
                <|
                    text "Ok"
            }
        ]


placesSelectionGrid : List String -> Element Msg
placesSelectionGrid selectedPlaces =
    wrappedRow [ width fill ]
        (List.map (placeSelection selectedPlaces) availablePlaces)


placeSelection : List String -> String -> Element Msg
placeSelection selectedPlaces place =
    checker2 (List.member place selectedPlaces) place


checker2 : Bool -> String -> Element Msg
checker2 selected place =
    el
        [ Element.Events.onClick (TogglePlace place)
        , paddingXY 10 10
        , width (px 100)
        ]
    <|
        Input.checkbox []
            { onChange = NoOp
            , icon = Input.defaultCheckbox
            , label = Input.labelRight [] (text place)
            , checked = selected
            }


availablePlaces =
    [ "Stockholm"
    , "Göteborg"
    , "Malmö"
    , "Uppsala"
    , "Västerås"
    , "Örebro"
    , "Linköping"
    , "Helsingborg"
    , "Jönköping"
    , "Norrköping"
    , "Lund"
    , "Umeå"
    , "Gävle"
    , "Borås"
    , "Södertälje"
    , "Eskilstuna"
    , "Halmstad"
    , "Växjö"
    , "Karlstad"
    , "Sundsvall"
    , "Östersund"
    , "Trollhättan"
    , "Luleå"
    , "Borlänge"
    , "Kristianstad"
    , "Kalmar"
    , "Falun"
    , "Skövde"
    , "Karlskrona"
    , "Skellefteå"
    , "Uddevalla"
    , "Varberg"
    , "Örnsköldsvik"
    , "Landskrona"
    , "Nyköping"
    , "Motala"
    ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
