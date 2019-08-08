module UnderstandingTime exposing (Model, Msg(..), main, update, view)

import Browser
import Html
import Iso8601
import Task
import Time exposing (Month(..))


type alias Model =
    { millis : Int
    , zone : Time.Zone
    , zoneName : String
    }


type Msg
    = SetZone Time.Zone
    | SetZoneName Time.ZoneName


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { millis = 1576238400000
      , zone = Time.utc
      , zoneName = "Unknown"
      }
    , Cmd.batch
        [ Task.perform SetZone Time.here
        , Task.perform SetZoneName Time.getZoneName
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetZone zone ->
            ( { model | zone = zone }, Cmd.none )

        SetZoneName zoneName ->
            ( { model | zoneName = z2s zoneName }, Cmd.none )


z2s : Time.ZoneName -> String
z2s zoneName =
    case zoneName of
        Time.Name string ->
            string

        Time.Offset int ->
            String.fromInt int


view : Model -> Html.Html Msg
view model =
    let
        time =
            Time.millisToPosix model.millis

        converted =
            case Iso8601.toTime "2019-12-13T12:00:00.000Z" of
                Ok res ->
                    res

                Err e ->
                    Time.millisToPosix 0
    in
    Html.div []
        [ Html.text "UTC"
        , Html.text <| " " ++ timeToString Time.utc time
        , Html.br [] []
        , Html.text model.zoneName
        , Html.text <| " " ++ timeToString model.zone time
        , Html.br [] []
        , Html.text <| Iso8601.fromTime time
        , Html.br [] []
        , Html.text <| Iso8601.fromTime converted
        ]


timeToString : Time.Zone -> Time.Posix -> String
timeToString zone timeStamp =
    timeToDate zone timeStamp
        ++ " "
        ++ timeToHourMinute zone timeStamp


timeToDate : Time.Zone -> Time.Posix -> String
timeToDate zone timeStamp =
    String.join "-" <| List.map (stringFromPosix zone timeStamp) [ Time.toYear, toIntMonth, Time.toDay ]


timeToHourMinute : Time.Zone -> Time.Posix -> String
timeToHourMinute zone timeStamp =
    String.join ":" <| List.map (stringFromPosix zone timeStamp) [ Time.toHour, Time.toMinute ]


stringFromPosix : Time.Zone -> Time.Posix -> (Time.Zone -> Time.Posix -> Int) -> String
stringFromPosix zone time fn =
    fn zone time |> twoCharacterInt


toIntMonth : Time.Zone -> Time.Posix -> Int
toIntMonth zone time =
    case Time.toMonth zone time of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


twoCharacterInt : Int -> String
twoCharacterInt n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n
