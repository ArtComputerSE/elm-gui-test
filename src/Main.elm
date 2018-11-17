module Main exposing (main)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)

channelPanel : List String -> Element msg
channelPanel channels =
    column
        [ height fill
        , width <| fillPortion 1
        , Background.color <| rgb255 92 99 118
        , Font.color <| rgb255 255 255 255
        ]
        <| List.map channelEl channels

channelEl : String -> Element msg
channelEl channelName = el [ paddingXY 15 5, width fill ] <| text ("#" ++ channelName)

chatPanel : Element msg
chatPanel =
    column [ height fill, width <| fillPortion 5 ]
        [ text "chat" ]


main : Html msg
main =
    layout [] <|
        row [ height fill, width fill ]
            [ channelPanel channelList
            , chatPanel
            ]

channelList : List String
channelList =
    [
    "extra"
    ,"general"
    ,"random"
    ]