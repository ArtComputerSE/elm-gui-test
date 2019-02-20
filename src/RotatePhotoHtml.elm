module RotatePhotoHtml exposing (Message(..), main)

import Html exposing (Html, div, img)
import Html.Attributes exposing (src, style)


type Message
    = NoOp


main : Html Message
main =
    div [ style "display" "flex", style "flex-wrap" "wrap" ]
        images


images : List (Html Message)
images =
    [ anImage portraitNotOk 0
    , anImage portraitOk 0
    , anImage portraitNotOk 270
    , anImage onTheCouch 90
    ]


anImage : String -> Int -> Html Message
anImage url rotation =
    {- div
       [ style "max-width" (String.fromInt w ++ "px")
       , style "height" "300px"
       ]
    -}
    div (enclosingAttributes rotation)
        [ img
            ([ src url
             , style "max-width" "300px"
             , style "max-height" "300px"
             ]
                ++ rotationAttribute rotation
            )
            []
        ]


enclosingAttributes rotation =
    case rotation of
        90 ->
            [ style "max-width" "225px", style "height" "300px" ]

        270 ->
            [ style "max-width" "225px", style "height" "300px" ]

        _ ->
            [ style "max-width" "300px", style "height" "225px" ]


rotationAttribute : Int -> List (Html.Attribute Message)
rotationAttribute rotation =
    case rotation of
        90 ->
            [ style "transform" ("rotate(" ++ String.fromInt rotation ++ "deg) translateY(38px) translateX(38px)") ]

        270 ->
            [ style "transform" ("rotate(" ++ String.fromInt rotation ++ "deg) translateY(-38px) translateX(-38px)") ]

        _ ->
            []


portraitOk =
    "https://storage.googleapis.com/lustcontact1/ff21f023-7c72-4602-9005-ecbd16365d8c2017-12-08+19.04.18.jpg"


portraitNotOk =
    "https://storage.googleapis.com/lustcontact1/39915826-979e-49df-bf44-a3045f0f8bb02017-03-17+17.38.58.jpg"


onTheCouch =
    "https://storage.googleapis.com/lustcontact1/176191f8-62eb-4304-9853-9ff8a13ae5fe2017-10-19+21.48.43.jpg"
