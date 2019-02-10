module SelectE exposing (Msg(..), Props, State, init, view)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Element.Keyed as Keyed
import Element.Lazy as Lazy exposing (..)
import FG exposing (..)
import FontAwesome.Light as Light
import FontAwesome.Solid as Solid
import FontAwesome.Transforms as Transforms
import IconE as Icon exposing (Icon)
import List.Extra
import MiscE


type alias State =
    { open : Bool
    , filter : String
    , index : Int
    }


init : State
init =
    { open = False
    , filter = ""
    , index = -1
    }


type alias Props key data msg =
    { state : State
    , options : List ( key, data )
    , selected : Maybe key
    , toString : data -> String
    , bind : Msg key -> msg
    , placeholder : String
    }


type Msg key
    = SetState State
    | Selected key


setFilter : State -> String -> Msg key
setFilter state string =
    SetState { open = True, filter = string, index = -1 }


open : State -> Msg key
open state =
    SetState { state | open = True }


close : State -> Msg key
close state =
    SetState init


keydown : Props key data msg -> String -> Msg key
keydown props key =
    let
        state =
            props.state
    in
    case Debug.log "key" key of
        "ArrowDown" ->
            if state.index < List.length (props |> filteredOptions) - 1 then
                SetState { state | index = state.index + 1 }

            else
                SetState state

        "ArrowUp" ->
            if state.index > 0 then
                SetState { state | index = state.index - 1 }

            else
                SetState state

        "Enter" ->
            case props |> filteredOptions |> List.Extra.getAt state.index of
                Just item ->
                    Selected (Tuple.first item)

                Nothing ->
                    SetState state

        _ ->
            SetState state


setIndex state index =
    SetState { state | index = index }


type alias ModifierKeys =
    { ctrl : Bool
    , shift : Bool
    , alt : Bool
    , meta : Bool
    }


filteredOptions props =
    props.options
        |> List.filter
            (\( key, item ) ->
                props.toString item
                    |> String.toLower
                    |> String.contains (String.toLower props.state.filter)
            )


view : Props key data msg -> Element msg
view props =
    column []
        [ Input.text
            [ padding 10
            , fontSize.m
            , onFocus (open props.state)
            , onLoseFocus (close props.state)
            , width (px 200)
            , MiscE.onKeyDown (keydown props)
            , inFront
                (Icon.view
                    [ Font.color c.darkPrimary
                    , alignRight
                    , centerY
                    , padding 10
                    ]
                    Solid.caretDown
                )
            , below
                (if props.state.open then
                    column
                        [ width fill
                        , Border.shadow { blur = 5, color = rgba 0 0 0 0.3, offset = ( 0, 3 ), size = 0 }
                        , Border.width 1
                        , Border.color c.grey
                        ]
                        (props
                            |> filteredOptions
                            |> List.indexedMap
                                (\i ( key, item ) ->
                                    el
                                        [ width fill
                                        , padding 5
                                        , pointer
                                        , onMouseDown (Selected key)
                                        , onMouseEnter (setIndex props.state i)
                                        , Background.color
                                            (if i == props.state.index then
                                                c.lightPrimary

                                             else
                                                c.white
                                            )
                                        ]
                                        (item |> props.toString |> MiscE.highlight props.state.filter)
                                )
                        )

                 else
                    none
                )
            ]
            { text = props.state.filter
            , label = Input.labelHidden ""
            , placeholder =
                Just
                    (case props.selected |> Maybe.andThen (\selected -> props.options |> List.Extra.find (\( key, data ) -> key == selected)) of
                        Just ( key, data ) ->
                            Input.placeholder [ Font.color c.black ] (text (props.toString data))

                        Nothing ->
                            Input.placeholder [] (text props.placeholder)
                    )
            , onChange = setFilter props.state
            }
        ]
        |> map props.bind
