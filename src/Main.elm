module Main exposing (main)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type alias Message =
    { author : String, time : String, text : String }


channelPanel : List String -> String -> Element msg
channelPanel channels activeChannel =
    column
        [ height fill
        , width <| fillPortion 1
        , Background.color <| rgb255 92 99 118
        , Font.color <| rgb255 255 255 255
        ]
    <|
        List.map (channelElement activeChannel) channels


channelElement : String -> String -> Element msg
channelElement activeChannel channelName =
    el (channelAttribute (activeChannel == channelName)) <| text ("#" ++ channelName)


channelAttribute : Bool -> List (Attribute msg)
channelAttribute active =
    if active then
        [ Background.color <| rgb255 117 179 201, Font.bold ] ++ [ paddingXY 15 5, width fill ]

    else
        [ paddingXY 15 5, width fill ]


chatPanel : String -> List Message -> Element msg
chatPanel activeChannel messageList =
    column [ height fill, width <| fillPortion 5 ]
        [ header activeChannel
        , messagePanel messageList
        , footer
        ]


header channel =
    row
        [ width fill
        , paddingXY 20 5
        , Border.widthEach { bottom = 1, top = 0, left = 0, right = 0 }
        , Border.color <| rgb255 200 200 200
        ]
        [ el [] <| text ("#" ++ channel)
        , Input.button
            [ padding 5
            , alignRight
            , Border.width 1
            , Border.rounded 3
            , Border.color <| rgb255 200 200 200
            ]
            { onPress = Nothing
            , label = text "Search"
            }
        ]


messagePanel : List Message -> Element msg
messagePanel messageList =
    column [ padding 10, spacingXY 0 20, scrollbarY ] <| List.map messageEntry messages


messageEntry : Message -> Element msg
messageEntry message =
    column [ width fill, spacingXY 0 5 ]
        [ row [ spacingXY 10 0 ]
            [ el [ Font.bold ] <| text message.author, text message.time ]
        , paragraph [] [ text message.text ]
        ]


footer =
    el [ alignBottom, padding 20, width fill ] <| footerContent


footerContent =
    row
        [ spacingXY 2 0
        , width fill
        , Border.width 2
        , Border.rounded 4
        , Border.color <| rgb255 200 200 200
        ]
        [ el
            [ padding 5
            , Border.widthEach { right = 2, left = 0, top = 0, bottom = 0 }
            , Border.color <| rgb255 200 200 200
            , mouseOver [ Background.color <| rgb255 86 182 139 ]
            ]
          <|
            text "+"
        , el [ Background.color <| rgb255 255 255 255 ] none
        ]


main : Html msg
main =
    layout [] <|
        row [ height fill, width fill ]
            [ channelPanel channelList "general"
            , chatPanel "general" messages
            ]


channelList : List String
channelList =
    [ "extra"
    , "general"
    , "random"
    , "commodities"
    ]


messages : List Message
messages =
    [ Message "Lovecraft" "1928-05-13" "Return from the crypt."
    , Message "Shakespear" "1612-01-14" "Much ado about nothing."
    ]
