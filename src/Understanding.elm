module Understanding exposing (main)

import Element exposing (Color, Element, alignLeft, alignRight, centerX, column, el, fill, height, layout, paddingXY, px, rgba, row, shrink, spacingXY, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type Message
    = SearchProfiles
    | FetchMyProfile
    | Logout


main : Html Message
main =
    layout [ height fill ] <|
        column [ height fill, width fill ]
            [ navbar
            , wrappedRow [ Border.width 1, width shrink, centerX ]
                [ content
                ]
            ]


content =
    column [ Border.width 1, width fill ]
        [ line
        , line
        , line
        , line
        , line
        , line
        ]


line =
    wrappedRow [ Border.width 1, width fill ]
        [ column [ Border.width 1, width (px 100), height (px 100), Background.color lightOrange ]
            []
        , column [ Border.width 1, width (px 100), height (px 100), Background.color lightRed ]
            []
        , column [ Border.width 1, width (px 100), height (px 100), Background.color lightOrange ]
            []
        , column [ Border.width 1, width (px 100), height (px 100), Background.color lightRed ]
            []
        , column [ Border.width 1, width (px 100), height (px 100), Background.color lightOrange ]
            []
        ]


navbar =
    row
        [ Border.width 1
        , width fill
        , paddingXY 60 10
        , spacingXY 20 0
        ]
        [ el [ alignLeft ] <| text "Program name"
        , el [] <| actionButton "Profiles" SearchProfiles
        , el [ centerX, Font.underline, Events.onClick FetchMyProfile ] <| text "user name"
        , el [ alignRight ] <| actionButton "Log out" Logout
        ]


actionButton : String -> Message -> Element Message
actionButton label msg =
    Input.button buttonAttributes
        { onPress = Just msg
        , label = el [ centerX ] <| text label
        }


buttonAttributes =
    [ Border.rounded 3
    , Border.widthEach { bottom = 3, top = 0, right = 0, left = 0 }
    , Font.bold
    , paddingXY 20 6
    , alignRight
    ]


lightRed : Color
lightRed =
    rgba (239 / 255) (41 / 255) (41 / 255) 1.0


lightOrange : Color
lightOrange =
    rgba (252 / 255) (175 / 255) (62 / 255) 1.0
