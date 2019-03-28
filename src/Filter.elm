module Places exposing (Model, Msg(..), main)

import AllColors exposing (themeColor0, themeColor1)
import Browser
import Element exposing (Attribute, Element, alignRight, centerX, centerY, clip, column, el, fill, height, inFront, layout, maximum, moveLeft, moveUp, none, paddingXY, px, row, spacingXY, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type PersonSex
    = Male
    | Female
    | Trans
    | SecretSex


type PersonOrientation
    = Hetero
    | Bi
    | Homo
    | SecretOrientation


type Msg
    = AddButtonPressed
    | DoneSelecting
    | ToggleCouples Bool
    | ToggleSex PersonSex
    | ToggleOrientation PersonOrientation
    | TogglePlace String
    | UpdateMinAge String
    | UpdateMaxAge String
    | NoOp Bool


type alias Model =
    { couples : Bool
    , sexSelections : List ( PersonSex, Bool )
    , orientationSelections : List ( PersonOrientation, Bool )
    , minAge : Int
    , maxAge : Int
    , places : List String
    , showPlacesSelection : Bool
    }


init : Model
init =
    { couples = False
    , sexSelections = [ ( Male, False ), ( Female, False ), ( Trans, False ), ( SecretSex, False ) ]
    , orientationSelections = [ ( Hetero, False ), ( Bi, False ), ( Homo, False ), ( SecretOrientation, False ) ]
    , minAge = 18
    , maxAge = 99
    , places = []
    , showPlacesSelection = False
    }


listGet : k -> List ( k, v ) -> v -> v
listGet key list default =
    case list of
        [] ->
            default

        ( k, v ) :: tail ->
            if k == key then
                v

            else
                listGet key tail default


listPut : k -> v -> List ( k, v ) -> List ( k, v )
listPut key value list =
    case list of
        [] ->
            [ ( key, value ) ]

        ( k, v ) :: tail ->
            if k == key then
                ( key, value ) :: tail

            else
                ( k, v ) :: listPut key value tail


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddButtonPressed ->
            { model | showPlacesSelection = True }

        DoneSelecting ->
            { model | showPlacesSelection = False }

        ToggleCouples _ ->
            { model | couples = not model.couples }

        ToggleSex sex ->
            let
                newValue =
                    not (listGet sex model.sexSelections False)
            in
            { model | sexSelections = listPut sex newValue model.sexSelections }

        ToggleOrientation orientation ->
            let
                newValue =
                    not (listGet orientation model.orientationSelections False)
            in
            { model | orientationSelections = listPut orientation newValue model.orientationSelections }

        UpdateMinAge n ->
            let
                newValue =
                    Maybe.withDefault model.minAge <| String.toInt n
            in
            { model | minAge = newValue }

        UpdateMaxAge n ->
            let
                newValue =
                    Maybe.withDefault model.maxAge <| String.toInt n
            in
            { model | maxAge = newValue }

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
            [ viewFilterControl model ]


viewFilterControl : Model -> Element Msg
viewFilterControl model =
    row
        [ width fill
        , Background.color themeColor0
        ]
        [ column []
            [ couplesSelector model
            , wrappedRow []
                [ person1 model
                , person2 model
                ]
            ]
        , viewPlacesControl model
        ]


person1 model =
    column [ width fill ]
        [ sexSelector model
        , orientationSelector model
        , ageRange model
        ]


person2 model =
    if model.couples then
        column [ width fill ]
            [ sexSelector model
            , orientationSelector model
            , ageRange model
            ]

    else
        none


couplesSelector : Model -> Element Msg
couplesSelector model =
    el
        [ Element.Events.onClick (ToggleCouples model.couples)
        , paddingXY 10 10
        , width (px 100)
        ]
    <|
        Input.checkbox []
            { onChange = NoOp
            , icon = Input.defaultCheckbox
            , label = Input.labelRight [] (text "Par")
            , checked = model.couples
            }


sexSelector model =
    row [ Border.width 1 ]
        (List.map sexChecker model.sexSelections)


sexChecker ( sex, selected ) =
    el
        [ Element.Events.onClick (ToggleSex sex)
        , paddingXY 10 10
        , width (px 100)
        ]
    <|
        Input.checkbox []
            { onChange = NoOp
            , icon = Input.defaultCheckbox
            , label = Input.labelRight [] (text (sexToString sex))
            , checked = selected
            }


sexToString : PersonSex -> String
sexToString sex =
    case sex of
        Female ->
            "Kvinna"

        Male ->
            "Man"

        Trans ->
            "Trans"

        SecretSex ->
            "Hemligt"


orientationSelector model =
    row [ Border.width 1 ]
        (List.map orientationChecker model.orientationSelections)


orientationChecker ( orientation, selected ) =
    el
        [ Element.Events.onClick (ToggleOrientation orientation)
        , paddingXY 10 10
        , width (px 100)
        ]
    <|
        Input.checkbox []
            { onChange = NoOp
            , icon = Input.defaultCheckbox
            , label = Input.labelRight [] (text (orientationToString orientation))
            , checked = selected
            }


orientationToString : PersonOrientation -> String
orientationToString orientation =
    case orientation of
        Hetero ->
            "Hetero"

        Bi ->
            "Bi"

        Homo ->
            "Homo"

        SecretOrientation ->
            "Hemlig"


ageRange model =
    row []
        [ el [] <| text "Ålder: "
        , Input.text [ width (px 50) ]
            { onChange = UpdateMinAge
            , text = String.fromInt model.minAge
            , placeholder = Nothing
            , label = Input.labelLeft [ centerY ] <| text "Min"
            }
        , Input.text [ width (px 50) ]
            { onChange = UpdateMaxAge
            , text = String.fromInt model.maxAge
            , placeholder = Nothing
            , label = Input.labelLeft [ centerY ] <| text "Max"
            }
        ]


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
    wrappedRow
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
