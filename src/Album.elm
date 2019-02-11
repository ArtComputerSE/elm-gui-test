module Album exposing (Message(..), main)

import Element exposing (column, fill, height, layout, row, width)
import Element.Border as Border
import Element.Input as Input
import File exposing (File)
import Html exposing (Html)


type Message
    = GotFiles (List File)


main : Html Message
main =
    layout [ height fill ] <|
        column [ height fill, width fill ]
            [ row [ Border.width 1, width fill ]
                [ content
                ]
            ]


content =
    Input.button
