module MiscE exposing (highlight, keyDecoder, onKeyDown, toRegex)

import Element exposing (Element, el, htmlAttribute, row, text)
import Element.Background as Background
import Element.Border as Border
import Html.Events
import Json.Decode as Decode
import List.Extra
import Regex


onKeyDown : (String -> msg) -> Element.Attribute msg
onKeyDown tagger =
    htmlAttribute (Html.Events.on "keydown" (keyDecoder |> Decode.map tagger))


keyDecoder =
    Decode.field "key" Decode.string


toRegex : String -> Regex.Regex
toRegex string =
    if string == "" then
        Regex.never

    else
        Regex.fromStringWith { caseInsensitive = True, multiline = False } string |> Maybe.withDefault Regex.never


highlight : String -> String -> Element msg
highlight substring string =
    let
        regex =
            toRegex substring

        matches =
            Regex.find regex string |> List.map (\match -> el [ Border.rounded 2 ] (text match.match))

        rest =
            Regex.split regex string |> List.map text
    in
    row [] (List.Extra.interweave rest matches)
