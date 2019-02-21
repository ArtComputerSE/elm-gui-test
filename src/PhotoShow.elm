module PhotoShow exposing (Message(..), main)

{-
   How do I present images nicely and how do I put controls around
   them?
-}

import Element
    exposing
        ( Element
        , alignBottom
        , alignLeft
        , alignRight
        , alignTop
        , centerX
        , column
        , el
        , fill
        , height
        , image
        , layout
        , maximum
        , paddingEach
        , paddingXY
        , row
        , shrink
        , spacingXY
        , text
        , width
        )
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)


type Message
    = NoOp


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
    , Photo 500 375 landscape
    , Photo 500 375 landscape
    ]


photoShow : List Photo -> Element Message
photoShow photos =
    column [ spacingXY 10 10 ] <| List.map showPhotoRow <| chunkList4 photos (Photo 375 500 empty)


showPhotoRow : List Photo -> Element Message
showPhotoRow list4 =
    row [] <| List.map showPhoto list4


showPhoto : Photo -> Element Message
showPhoto photo =
    column
        [ width fill
        , height fill
        , Border.widthEach { top = 1, bottom = 1, left = 1, right = 1 }
        , Border.rounded 5
        ]
        [ row
            [ width fill
            , alignTop
            , paddingEach { top = 5, bottom = 1, left = 5, right = 5 }
            ]
            [ Input.button
                [ width shrink
                , Border.width 1
                , Border.rounded 5
                , paddingXY 10 3
                , centerX
                ]
                { onPress = Just NoOp
                , label = el [] <| text "X"
                }
            ]
        , row []
            [ image
                [ width (fill |> maximum photo.width)
                , height (fill |> maximum photo.height)
                , Border.width 5
                , Border.rounded 5
                ]
                { src = photo.url
                , description = "photo shown here"
                }
            ]
        , row
            [ width fill
            , alignBottom
            , Border.widthEach { top = 0, bottom = 0, left = 0, right = 0 }
            , paddingEach { top = 1, bottom = 5, left = 5, right = 5 }
            ]
            [ Input.button
                [ width shrink
                , Border.width 1
                , paddingXY 3 3
                , Border.rounded 5
                , alignLeft
                ]
                { onPress = Just NoOp
                , label = el [] <| text "<="
                }
            , Input.button
                [ width shrink
                , Border.width 1
                , paddingXY 3 3
                , Border.rounded 5
                , alignRight
                ]
                { onPress = Just NoOp
                , label = el [] <| text "=>"
                }
            ]
        ]


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
            ]
            [ photoShow photosData ]
