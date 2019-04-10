module DatePicker exposing
    ( Cal
    , Column
    , Window
    , clearSelection
    , columns
    , firstDayOfWeek
    , greyOut
    , init
    , lastDayOfWeek
    , next
    , nextWindow
    , pad
    , paddedDates
    , prev
    , prevWindow
    , resetWindow
    , select
    , windowStart
    )

{- https://package.elm-lang.org/packages/justinmimbs/date/latest/ -}

import Date exposing (Date)
import Time exposing (Month(..), Posix, Weekday(..))


type alias Window =
    { year : Int
    , month : Time.Month
    }


windowStart : Window -> Date
windowStart window =
    Date.fromCalendarDate window.year window.month 1


type alias Cal =
    { window : Window
    , today : Date
    , selected : Maybe Date
    }


greyOut : Cal -> Date -> Bool
greyOut cal date =
    not <|
        ((Date.month date == cal.window.month) && (Date.year date == cal.window.year))


init : Date -> Cal
init today =
    let
        month =
            Date.month today

        year =
            Date.year today
    in
    { window = { year = year, month = month }
    , today = today
    , selected = Nothing
    }


resetWindow : Cal -> Cal
resetWindow cal =
    init cal.today
        |> (\new -> { new | selected = cal.selected })


firstDayOfWeek : Time.Weekday
firstDayOfWeek =
    Time.Sun


lastDayOfWeek : Time.Weekday
lastDayOfWeek =
    Time.Sat


type alias Column =
    { weekday : Time.Weekday, days : List Date }


paddedDates : Cal -> List Date
paddedDates cal =
    Date.range Date.Day
        1
        (cal.window |> windowStart)
        (cal.window |> nextWindow |> windowStart)
        |> pad firstDayOfWeek -1
        |> (List.reverse >> pad lastDayOfWeek 1 >> List.reverse)


columns : Cal -> List Column
columns cal =
    cal
        |> paddedDates
        |> (\dates ->
                let
                    columnFor day =
                        { weekday = day
                        , days = List.filter (\d -> Date.weekday d == day) dates
                        }
                in
                [ Time.Sun
                , Time.Mon
                , Time.Tue
                , Time.Wed
                , Time.Thu
                , Time.Fri
                , Time.Sat
                ]
                    |> List.map columnFor
           )


pad : Date.Weekday -> Int -> List Date -> List Date
pad target step dates =
    case dates of
        [] ->
            dates

        first :: rest ->
            if Date.weekday first /= target then
                pad target step <| Date.add Date.Days step first :: dates

            else
                dates


select : Date -> Cal -> Cal
select date cal =
    { cal | selected = Just date }


clearSelection : Cal -> Cal
clearSelection cal =
    { cal | selected = Nothing }


next : Cal -> Cal
next cal =
    { cal | window = nextWindow cal.window }


prev : Cal -> Cal
prev cal =
    { cal | window = prevWindow cal.window }


nextWindow : Window -> Window
nextWindow window =
    Date.add Date.Months 1 (Date.fromCalendarDate window.year window.month 1)
        |> (\date -> { year = Date.year date, month = Date.month date })


prevWindow : Window -> Window
prevWindow window =
    Date.add Date.Months -1 (Date.fromCalendarDate window.year window.month 1)
        |> (\date -> { year = Date.year date, month = Date.month date })
