module Profile.ProfilePage exposing (Message(..), profilePage)

import AllColors exposing (themeColor1, themeColor2)
import Element exposing (Attribute, Element, alignLeft, alignTop, centerX, column, el, fill, fillPortion, height, layout, none, paddingXY, paragraph, px, row, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Profile.Profile as Profile exposing (PersonMartialStatus(..), PersonSex(..))


main : Html Message
main =
    layout [ height fill ] <|
        profilePage someProfile


someProfile : Profile.Profile
someProfile =
    { id = "some id"
    , userName = "The user name"
    , kind = Profile.SingleProfile
    , person1 = somePerson
    , person2 = Nothing
    , description = """
    Descriptions. Let's write a long one. Here goes :sdmfh sldhsjdf dsajkhfsdhfkjdshg jsdhfjhds fhjdsgf jadsg fhg df
    djksfh djskhfds fhsdjfksfhd kjsadhfkjshf sjkdfhdjkshf khdsf sdh
     sdjfh ksadfhkasjhfjksdahf jkashdf sd fhjsahfd kajsdhjfkj asdhfjkashdf jksdhf asfkjdh ajksdfhkashdf jakhfdsf
    dshfjkshfjk hj sdfjksdfjklds ksjd fjslkjsd flksadfjlsdjfkj hsjkfdh ahfjhasf kasjhfdjklashfkjahf jkshdf sdjfhjskfh
    sjhf jshf jsdfh kahf lakjhdfjhdfjahd fdhj fhjadhf jdha jshf jdha ödkf djkhf akjdfh jdh hjfhd
    dhfjdf hfdj jdshfjsdafkjhd fjsdhsjdf sdfhsjfjsdhf jkshdf kjsafhdkjshdfjhd fjhdsfkajhfjd hfjadf adjsfhjdsf j fdjak
    dsfh jsdhf kajhsfjhsda jkhdf kajshdfjk djfh ajhdfjdhf jdhf ahjd fdhja  faö j dhjfj fjd fjd
    """
    }


somePerson : Profile.Person
somePerson =
    { id = "some id"
    , sex = Profile.Female
    , height = Just 190
    , weight = Just 87
    , martialStatus = Just Married
    , description = """
        dsfhjd fhsgd hfgsdhf gahjsg hagsfhjgsad hjfsgdhfj gsfjhjasg fgasjhf ajshfd jahsgdf afhj
        sdfnksfdshfjksdhfj khsd fjdhsf jkhdsjf khsdjfhdjk sjfhjkds fjkds fkdg fhjds fhjagf j
        """
    }


type Message
    = GoBack


profilePage : Profile.Profile -> Element Message
profilePage profile =
    column
        [ Element.explain Debug.todo
        , width fill
        ]
        [ topSection profile
        , personSection profile.person1
        ]


topSection : Profile.Profile -> Element Message
topSection profile =
    row
        [ spacingXY 10 10
        , paddingXY 5 5
        , width fill
        ]
        [ profilePicture
        , column [ alignTop, width fill ]
            [ row [ alignTop, width fill ] [ profileUserName profile.userName ]
            , row [] [ paragraph [] [ text profile.description ] ]
            ]
        ]


profilePicture =
    column [ alignLeft ]
        [ el
            [ Border.width 1
            , Border.rounded 3
            , width (px 200)
            , height (px 200)
            ]
            none
        ]


profileUserName userName =
    el
        profileHeadLine
    <|
        text userName


profileHeadLine =
    [ paddingXY 5 5
    , width fill
    , Background.color themeColor1
    , Border.color themeColor2
    , Border.rounded 3
    , Border.width 3
    ]


personSection : Profile.Person -> Element Message
personSection person =
    column [ width fill, paddingXY 5 5, spacingXY 5 5 ]
        [ row
            profileHeadLine
            [ el [ alignLeft ] <| text <| personSexToString person.sex
            ]
        , row [ width fill ] [ personDetails person ]
        , row [] [ paragraph [] [ text person.description ] ]
        ]


personDetails : Profile.Person -> Element Message
personDetails person =
    row [ width fill ]
        [ column personDetailsColumn
            [ personDetailsPoint "Ålder:" <| maybeAgeToString Nothing
            , personDetailsPoint "Civilstånd:" <| martialStatusToString person.martialStatus
            , personDetailsPoint "Längd:" <| maybeIntToString person.height
            , personDetailsPoint "Vikt:" <| maybeIntToString person.weight
            ]
        , column personDetailsColumn
            [ personDetailsPoint "Kropp:" ""
            , personDetailsPoint "Rakad:" "Ja"
            , personDetailsPoint "Ögonfärg:" "Svarta"
            , personDetailsPoint "Hår:" "Skallig"
            ]
        , column personDetailsColumn
            [ personDetailsPoint "Röker:" ""
            , personDetailsPoint "Dricker:" ""
            , personDetailsPoint "Tatuering:" ""
            , personDetailsPoint "Piercing:" ""
            ]
        ]


personDetailsPoint : String -> String -> Element Message
personDetailsPoint label value =
    if String.length value > 0 then
        row [ spacingXY 10 10 ]
            [ el [ Font.bold ] <| text label
            , el [] <| text value
            ]

    else
        row [ spacingXY 10 10 ]
            [ el [] <| text label
            , none
            ]


personDetailsColumn : List (Attribute Message)
personDetailsColumn =
    [ paddingXY 5 5
    , width (fillPortion 1)
    , Border.width 1
    , Border.color themeColor2
    , Border.rounded 3
    ]


personSexToString : Profile.PersonSex -> String
personSexToString sex =
    case sex of
        Female ->
            "Kvinna"

        Male ->
            "Man"

        Trans ->
            "Trans"

        Unknown ->
            "Hemligt kön"


martialStatusToString : Maybe Profile.PersonMartialStatus -> String
martialStatusToString status =
    case status of
        Nothing ->
            ""

        Just v ->
            case v of
                Married ->
                    "Gift"

                Apart ->
                    "Särbo"

                Partner ->
                    "Sambo"

                Single ->
                    "Single"


maybeAgeToString : Maybe Int -> String
maybeAgeToString maybe =
    case maybe of
        Nothing ->
            ""

        Just yearOfBirth ->
            String.fromInt (2019 - yearOfBirth)


maybeIntToString : Maybe Int -> String
maybeIntToString maybe =
    case maybe of
        Nothing ->
            ""

        Just n ->
            String.fromInt n
