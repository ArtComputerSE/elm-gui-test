module PhotoShow exposing (Message(..), main)

{-
   How do I present images nicely and how do I put controls around
   them?
-}

import AllColors exposing (themeColor0, themeColor1, themeColor2)
import Element exposing (Element, alignBottom, alignLeft, alignRight, alignTop, centerX, column, el, fill, height, image, layout, maximum, none, paddingEach, paddingXY, row, shrink, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)


type Message
    = NoOp Photo


type alias Photo =
    { width : Int
    , height : Int
    , url : String
    }


landscape =
    "https://storage.googleapis.com/lustcontact1/af2b00de-d66e-4a4a-87e4-c71e807c9e61.jpg"


portrait =
    "https://storage.googleapis.com/lustcontact1/dc88d286-9d65-43d8-8f23-ffaf624d6057.jpg"


empty =
    "https://storage.googleapis.com/lustcontact1/empty.jpg"


photosData =
    [ Photo 375 500 portrait
    , Photo 500 375 landscape
    , Photo 375 500 portrait
    , Photo 375 500 portrait
    , Photo 500 375 landscape
    ]


photoEdit : List Photo -> Element Message
photoEdit photos =
    column
        [ spacingXY 10 10
        , width fill
        ]
    <|
        List.map photoEditRow <|
            chunkList4 photos (Photo 375 500 empty)


photoShow : List Photo -> Element Message
photoShow photos =
    column
        [ spacingXY 10 10
        , width fill
        ]
    <|
        List.map photoShowRow <|
            chunkList4 photos (Photo 375 500 empty)


photoEditRow : List Photo -> Element Message
photoEditRow list4 =
    row [ spacingXY 5 0, paddingXY 5 0 ] <| List.map photoEditPhoto list4


photoShowRow : List Photo -> Element Message
photoShowRow list4 =
    row [ spacingXY 5 0, paddingXY 5 0 ] <| List.map photoShowPhoto list4


photoShowPhoto : Photo -> Element Message
photoShowPhoto photo =
    image
        [ width (fill |> maximum photo.width)
        , height (fill |> maximum photo.height)
        , Border.width 5
        , Border.rounded 5
        , Border.color themeColor2
        , Border.glow themeColor1 1
        ]
        { src = photo.url
        , description = "photo shown here"
        }


photoEditPhoto : Photo -> Element Message
photoEditPhoto photo =
    column
        [ width fill
        , height fill
        , Border.widthEach { top = 1, bottom = 1, left = 1, right = 1 }
        , Border.rounded 5
        ]
        [ topControl photo (photo.url == empty)
        , photoShowPhoto photo
        , bottomControls photo (photo.url == empty)
        ]


topControl : Photo -> Bool -> Element Message
topControl photo showControl =
    row
        [ width fill
        , alignTop
        , paddingEach { top = 5, bottom = 1, left = 5, right = 5 }
        , Border.color themeColor2
        , Border.glow themeColor1 1
        ]
        [ if showControl then
            none

          else
            deleteButton photo
        ]


bottomControls : Photo -> Bool -> Element Message
bottomControls photo showControls =
    row
        [ width fill
        , alignBottom
        , Border.widthEach { top = 0, bottom = 0, left = 0, right = 0 }
        , paddingEach { top = 1, bottom = 5, left = 5, right = 5 }
        , Border.color themeColor2
        , Border.glow themeColor1 1
        ]
        [ if showControls then
            none

          else
            leftButton photo
        , if showControls then
            none

          else
            rightButton photo
        ]


deleteButton : Photo -> Element Message
deleteButton photo =
    Input.button
        [ width shrink
        , Border.width 1
        , Border.rounded 5
        , paddingXY 10 3
        , centerX
        , Border.color themeColor2
        , Border.glow themeColor1 1
        ]
        { onPress = Just (NoOp photo)
        , label = el [] <| text "X"
        }


leftButton : Photo -> Element Message
leftButton photo =
    Input.button
        [ width shrink
        , Border.width 1
        , paddingXY 3 3
        , Border.rounded 5
        , alignLeft
        , Border.color themeColor2
        , Border.glow themeColor1 1
        ]
        { onPress = Just (NoOp photo)
        , label = el [] <| text "<="
        }


rightButton : Photo -> Element Message
rightButton photo =
    Input.button
        [ width shrink
        , Border.width 1
        , paddingXY 3 3
        , Border.rounded 5
        , alignRight
        , Border.color themeColor2
        , Border.glow themeColor1 1
        ]
        { onPress = Just (NoOp photo)
        , label = el [] <| text "=>"
        }


chunkList4 : List a -> a -> List (List a)
chunkList4 list pad =
    case list of
        a :: b :: c :: d :: tail ->
            [ [ a, b, c, d ] ] ++ chunkList4 tail pad

        a :: b :: c :: [] ->
            [ [ a, b, c, pad ] ]

        a :: b :: [] ->
            [ [ a, b, pad, pad ] ]

        a :: [] ->
            [ [ a, pad, pad, pad ] ]

        [] ->
            []


main : Html Message
main =
    layout [ height fill ] <|
        column
            [ width fill
            , spacingXY 0 30
            , Background.color themeColor0
            ]
            [ photoEdit photosData
            , photoShow photosData
            ]
