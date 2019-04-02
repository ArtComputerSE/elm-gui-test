module Places exposing (Model, Msg(..), main)

import AllColors exposing (themeColor0)
import Browser
import Element exposing (Attribute, Element, centerX, centerY, column, el, fill, height, layout, none, paddingXY, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


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
    , orientationSelections : List PersonOrientation
    , minAge : Int
    , maxAge : Int
    }


type CouplesSelection
    = MixedCouple
    | MaleCouple
    | FemaleCouple
    | SingleMale
    | SingleFemale


type Msg
    = NoOp Bool
    | UpdateCouplesSelection CouplesSelection
    | ToggleOrientation Person1Or2 PersonOrientation
    | UpdateMinAge Person1Or2 String
    | UpdateMaxAge Person1Or2 String


type alias Model =
    { couples : Maybe CouplesSelection
    , person1Selection : PersonSelections
    , person2Selection : PersonSelections
    }


defaultPersonSelections : Person1Or2 -> PersonSelections
defaultPersonSelections p12 =
    { owner = p12
    , orientationSelections = []
    , minAge = 18
    , maxAge = 99
    }


init : Model
init =
    { couples = Nothing
    , person1Selection = defaultPersonSelections Person1
    , person2Selection = defaultPersonSelections Person2
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp _ ->
            model

        UpdateCouplesSelection selection ->
            { model | couples = Just selection }

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
        row
            [ width fill
            , centerX
            , paddingXY 30 30
            , Background.color themeColor0
            , Font.size 16
            , spacing 16
            ]
            [ couplesSelector model
            , personsSelector model
            ]


couplesSelector : Model -> Element Msg
couplesSelector model =
    Input.radio
        [ spacing 12
        , Background.color themeColor0
        ]
        { selected = model.couples
        , onChange = \new -> UpdateCouplesSelection new
        , label = Input.labelAbove [ Font.size 14, paddingXY 0 12 ] (text "Vilken typ av profil vill su söka på?")
        , options =
            [ Input.option MixedCouple (text "Mixat par")
            , Input.option MaleCouple (text "Manspar")
            , Input.option FemaleCouple (text "Kvinnopar")
            , Input.option SingleMale (text "Enbart man")
            , Input.option SingleFemale (text "Enbart kvinna")
            ]
        }


personsSelector : Model -> Element Msg
personsSelector model =
    column
        [ width fill
        , spacing 16
        ]
        (case model.couples of
            Nothing ->
                [ none ]

            Just select ->
                case select of
                    MixedCouple ->
                        [ personSelector model.person1Selection "Egenskaper för mannen"
                        , personSelector model.person2Selection "Egenskaper för kvinnan"
                        ]

                    MaleCouple ->
                        [ personSelector model.person1Selection "Egenskaper för männen"
                        ]

                    FemaleCouple ->
                        [ personSelector model.person1Selection "Egenskaper för kvinnorna"
                        ]

                    SingleMale ->
                        [ personSelector model.person1Selection "Egenskaper för mannen"
                        ]

                    SingleFemale ->
                        [ personSelector model.person1Selection "Egenskaper för kvinnan"
                        ]
        )


personSelector : PersonSelections -> String -> Element Msg
personSelector personSelections str =
    column [ width fill ]
        [ el [] <| text <| str
        , orientationSelector personSelections.owner personSelections
        , ageRange personSelections.owner personSelections.minAge personSelections.maxAge
        ]


orientationSelector : Person1Or2 -> PersonSelections -> Element Msg
orientationSelector p12 personSelections =
    row [ Border.width 1 ]
        (List.map (orientationChecker p12 personSelections.orientationSelections) [ Hetero, Bi, Homo ])


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
        [ el [] <| text "Ålder "
        , Input.text [ width (px 70) ]
            { onChange = UpdateMinAge p12
            , text = String.fromInt currentMin
            , placeholder = Nothing
            , label = Input.labelLeft [ centerY ] <| text "mellan"
            }
        , Input.text [ width (px 70) ]
            { onChange = UpdateMaxAge p12
            , text = String.fromInt currentMax
            , placeholder = Nothing
            , label = Input.labelLeft [ centerY ] <| text " och "
            }
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
