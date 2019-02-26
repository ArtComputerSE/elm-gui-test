module ClickThisPlate exposing (Message(..), main)

import AllColors exposing (themeColor0)
import Element exposing (Element, centerX, column, fill, height, image, layout, paddingXY, px, row, shrink, spacingXY, text, width, wrappedRow)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type Message
    = NoOp


portrait =
    "https://storage.googleapis.com/lustcontact1/dc88d286-9d65-43d8-8f23-ffaf624d6057.jpg"


plates : List (Element Message)
plates =
    [ wrappedRow [ spacingXY 5 5 ]
        [ plate "Cougar 66"
        , plate "Shorty"
        , plate "A very long name"
        , plate "Some other"
        , plate "Shorty"
        , plate "A very long name"
        , plate "Some other"
        ]
    ]


plate : String -> Element Message
plate string =
    Input.button [ Border.width 1, width (px 200) ]
        { onPress = Just NoOp
        , label =
            row [ spacingXY 5 5 ]
                [ column [ paddingXY 10 10 ]
                    [ image [ height (px 50) ]
                        { src = portrait
                        , description = "photo"
                        }
                    ]
                , column
                    [ Font.size 12, paddingXY 10 10 ]
                    [ text string
                    , text "...."
                    , text "...."
                    , text "...."
                    ]
                ]
        }


main : Html Message
main =
    layout [ height fill ] <|
        column
            [ width shrink
            , centerX
            , paddingXY 30 30
            , Background.color themeColor0
            ]
            plates
