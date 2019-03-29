module Places exposing (Model, Msg(..), main)

import AllColors exposing (themeColor0, themeColor1)
import Browser
import Element exposing (Attribute, Element, alignRight, centerX, centerY, clip, column, el, fill, height, inFront, layout, maximum, moveLeft, moveUp, none, paddingXY, px, row, spaceEvenly, spacingXY, text, width, wrappedRow)
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


type Person1Or2
    = Person1
    | Person2


type alias PersonSelections =
    { owner : Person1Or2
    , sexSelections : List PersonSex
    , orientationSelections : List PersonOrientation
    , minAge : Int
    , maxAge : Int
    }


defaultPersonSelections : Person1Or2 -> PersonSelections
defaultPersonSelections p12 =
    { owner = p12
    , sexSelections = []
    , orientationSelections = []
    , minAge = 18
    , maxAge = 99
    }


type Msg
    = AddButtonPressed
    | DoneSelecting
    | TogglePlace String
    | ToggleCouples Bool
    | ToggleSex Person1Or2 PersonSex
    | ToggleOrientation Person1Or2 PersonOrientation
    | UpdateMinAge Person1Or2 String
    | UpdateMaxAge Person1Or2 String
    | NoOp Bool


type alias Model =
    { couples : Bool
    , person1Selection : PersonSelections
    , person2Selection : PersonSelections
    , places : List String
    , showPlacesSelection : Bool
    }


init : Model
init =
    { couples = False
    , person1Selection = defaultPersonSelections Person1
    , person2Selection = defaultPersonSelections Person2
    , places = []
    , showPlacesSelection = False
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddButtonPressed ->
            { model | showPlacesSelection = True }

        DoneSelecting ->
            { model | showPlacesSelection = False }

        ToggleCouples _ ->
            { model | couples = not model.couples }

        ToggleSex p12 sex ->
            case p12 of
                Person1 ->
                    { model | person1Selection = newSexSelection model.person1Selection sex }

                Person2 ->
                    { model | person2Selection = newSexSelection model.person2Selection sex }

        ToggleOrientation p12 orientation ->
            case p12 of
                Person1 ->
                    { model | person1Selection = newOrientationSelection model.person1Selection orientation }

                Person2 ->
                    { model | person2Selection = newOrientationSelection model.person2Selection orientation }

        UpdateMinAge p12 n ->
            case p12 of
                Person1 ->
                    { model | person1Selection = newMinAge model.person1Selection n }

                Person2 ->
                    { model | person2Selection = newMinAge model.person2Selection n }

        UpdateMaxAge p12 n ->
            case p12 of
                Person1 ->
                    { model | person1Selection = newMaxAge model.person1Selection n }

                Person2 ->
                    { model | person2Selection = newMaxAge model.person2Selection n }

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


newSexSelection personSelections sex =
    let
        newList =
            if List.member sex personSelections.sexSelections then
                List.filter (\s -> s /= sex) personSelections.sexSelections

            else
                sex :: personSelections.sexSelections
    in
    { personSelections | sexSelections = newList }


newOrientationSelection personSelections orientation =
    let
        newList =
            if List.member orientation personSelections.orientationSelections then
                List.filter (\s -> s /= orientation) personSelections.orientationSelections

            else
                orientation :: personSelections.orientationSelections
    in
    { personSelections | orientationSelections = newList }


newMinAge personSelections n =
    let
        newAge =
            toIntOrZero n personSelections.minAge
    in
    { personSelections | minAge = newAge }


newMaxAge personSelections n =
    let
        newAge =
            toIntOrZero n personSelections.maxAge
    in
    { personSelections | maxAge = newAge }


toIntOrZero n default =
    Maybe.withDefault default <|
        String.toInt
            (if String.length n > 0 then
                n

             else
                "0"
            )


view : Model -> Html Msg
view model =
    layout [ height fill ] <|
        column
            [ width fill
            , centerX
            , paddingXY 30 30
            , Background.color themeColor0
            , Font.size 16
            ]
            [ viewFilterControl model ]


viewFilterControl : Model -> Element Msg
viewFilterControl model =
    row
        [ width fill
        , Background.color themeColor0
        , spaceEvenly
        ]
        [ column [ width fill, Border.width 1 ]
            [ couplesSelector model
            , wrappedRow [ width fill ]
                [ person model.person1Selection
                , if model.couples then
                    person model.person2Selection

                  else
                    none
                ]
            ]
        , viewPlacesControl model
        ]


person : PersonSelections -> Element Msg
person personSelections =
    column [ width fill ]
        [ sexSelector personSelections.owner personSelections
        , orientationSelector personSelections.owner personSelections
        , ageRange personSelections.owner personSelections.minAge personSelections.maxAge
        ]


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


sexSelector : Person1Or2 -> PersonSelections -> Element Msg
sexSelector p12 personSelections =
    row [ Border.width 1 ]
        (List.map (sexChecker p12 personSelections.sexSelections) [ Male, Female, Trans, SecretSex ])


sexChecker : Person1Or2 -> List PersonSex -> PersonSex -> Element Msg
sexChecker p12 selections sex =
    el
        [ Element.Events.onClick (ToggleSex p12 sex)
        , paddingXY 10 10
        , width (px 100)
        ]
    <|
        Input.checkbox []
            { onChange = NoOp
            , icon = Input.defaultCheckbox
            , label = Input.labelRight [] (text (sexToString sex))
            , checked = List.member sex selections
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


orientationSelector : Person1Or2 -> PersonSelections -> Element Msg
orientationSelector p12 personSelections =
    row [ Border.width 1 ]
        (List.map (orientationChecker p12 personSelections.orientationSelections) [ Hetero, Bi, Homo, SecretOrientation ])


orientationChecker p12 selections orientation =
    el
        [ Element.Events.onClick (ToggleOrientation p12 orientation)
        , paddingXY 10 10
        , width (px 100)
        ]
    <|
        Input.checkbox []
            { onChange = NoOp
            , icon = Input.defaultCheckbox
            , label = Input.labelRight [] (text (orientationToString orientation))
            , checked = List.member orientation selections
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


ageRange : Person1Or2 -> Int -> Int -> Element Msg
ageRange p12 currentMin currentMax =
    row [ width fill ]
        [ el [] <| text "Ålder: "
        , Input.text [ width (px 70) ]
            { onChange = UpdateMinAge p12
            , text = String.fromInt currentMin
            , placeholder = Nothing
            , label = Input.labelLeft [ centerY ] <| text "Min"
            }
        , Input.text [ width (px 70) ]
            { onChange = UpdateMaxAge p12
            , text = String.fromInt currentMax
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
            , Font.size 16
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
