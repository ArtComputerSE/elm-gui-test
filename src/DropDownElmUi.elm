module DropDownElmUi exposing (decodeAncestorPropery, decodeGroupIdChanged, groupIdAttribute, onGroupFocusLoss)

import Element
import Html.Attributes
import Html.Events
import Json.Decode as Decode



{-
   I have a drop-down element and would like to close it when mouse clicked outside it. Does anybody know
   if this is doable without the use of JS ports?
-}


decodeAncestorPropery : Decode.Decoder p -> Decode.Decoder (List p)
decodeAncestorPropery decoder =
    Decode.oneOf
        [ decoder |> Decode.map (\p -> [ p ])
        , Decode.succeed []
        ]
        |> Decode.andThen
            (\ps ->
                Decode.oneOf
                    [ Decode.at [ "parentNode" ] (decodeAncestorPropery decoder)
                        |> Decode.andThen (\parents -> Decode.succeed (ps ++ parents))
                    , Decode.succeed ps
                    ]
            )


decodeGroupIdChanged : (String -> msg) -> Decode.Decoder msg
decodeGroupIdChanged msg =
    Decode.map2
        (\a b ->
            if List.head a /= List.head b then
                List.head a

            else
                Nothing
        )
        (Decode.at [ "target" ] <|
            decodeAncestorPropery (Decode.at [ "dataset", "groupId" ] Decode.string)
        )
        (Decode.at [ "relatedTarget" ] <|
            decodeAncestorPropery (Decode.at [ "dataset", "groupId" ] Decode.string)
        )
        |> Decode.andThen
            (\maybeChanged ->
                case maybeChanged of
                    Just a ->
                        Decode.succeed (msg a)

                    Nothing ->
                        Decode.fail "no change"
            )


onGroupFocusLoss : (String -> msg) -> Element.Attribute msg
onGroupFocusLoss msg =
    Element.htmlAttribute <|
        Html.Events.on "focusout" (decodeGroupIdChanged msg)


groupIdAttribute : String -> Element.Attribute msg
groupIdAttribute groupId =
    Element.htmlAttribute <| Html.Attributes.attribute "data-group-id" groupId



{-
   So your dorpdown element needs a unique `groupIdAttribute` and `onGroupFocusLoss` which needs a Msg constructor
    which will be passed the groupId.
-}
