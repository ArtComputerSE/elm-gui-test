module RotatePhoto exposing (Message(..), main)

import AllColors exposing (themeColor1)
import Element exposing (Attribute, Element, centerX, column, fill, height, image, layout, moveDown, paddingXY, px, rotate, row, shrink, spacingXY, text, width)
import Element.Border as Border
import Html exposing (Html)


type Message
    = NoOp


main : Html Message
main =
    layout [ height fill ] <|
        column [ width fill, spacingXY 10 10 ]
            [ row [ height shrink, width fill, Border.width 1, paddingXY 10 10 ]
                [ column [ centerX ]
                    [ text "Hello"
                    ]
                ]
            , row
                [ height (px 310)
                , Border.width 1
                , Border.color themeColor1
                ]
                images
            , row
                [ height (px 310)
                , Border.width 1
                , Border.color themeColor1
                ]
                images
            ]


images : List (Element Message)
images =
    [ anImage portraitNotOk 0 300 225
    , anImage portraitOk 0 225 300
    , anImage portraitNotOk 270 225 300
    , anImage onTheCouch 90 225 300
    ]


anImage url rotation w h =
    column [ width (px 300), height (px 300) ]
        [ image
            (imageSize (w > h) rotation)
            { src = url
            , description = ""
            }
        ]


imageSize : Bool -> Int -> List (Attribute Message)
imageSize landscape rotation =
    if landscape then
        if rotation == 0 then
            [ height (px 225), width (px 300) ]

        else
            [ height (px 300), width (px 225), rotate (Basics.degrees (toFloat rotation)), moveDown 38 ]

    else if rotation == 0 then
        [ height (px 300), width (px 225) ]

    else
        [ height (px 225), width (px 300), rotate (Basics.degrees (toFloat rotation)), moveDown 38 ]


portraitOk =
    "https://storage.googleapis.com/lustcontact1/ff21f023-7c72-4602-9005-ecbd16365d8c2017-12-08+19.04.18.jpg"


portraitNotOk =
    "https://storage.googleapis.com/lustcontact1/39915826-979e-49df-bf44-a3045f0f8bb02017-03-17+17.38.58.jpg"


onTheCouch =
    "https://storage.googleapis.com/lustcontact1/176191f8-62eb-4304-9853-9ff8a13ae5fe2017-10-19+21.48.43.jpg"
