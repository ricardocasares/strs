module Format exposing (compact, thousands, ago, clock)

import Time


{-| 950, 1.2k, 12k, 1.2M.
-}
compact : Int -> String
compact n =
    if n < 1000 then
        String.fromInt n

    else if n < 10000 then
        oneDecimal (toFloat n / 1000) ++ "k"

    else if n < 1000000 then
        String.fromInt (n // 1000) ++ "k"

    else
        oneDecimal (toFloat n / 1000000) ++ "M"


oneDecimal : Float -> String
oneDecimal x =
    let
        tenths =
            floor (x * 10)
    in
    if modBy 10 tenths == 0 then
        String.fromInt (tenths // 10)

    else
        String.fromInt (tenths // 10) ++ "." ++ String.fromInt (modBy 10 tenths)


{-| 2,026.
-}
thousands : Int -> String
thousands n =
    let
        digits =
            String.fromInt (abs n)

        grouped =
            String.reverse digits
                |> chunks
                |> String.join ","
                |> String.reverse
    in
    if n < 0 then
        "-" ++ grouped

    else
        grouped


chunks : String -> List String
chunks text =
    if String.length text <= 3 then
        [ text ]

    else
        String.left 3 text :: chunks (String.dropLeft 3 text)


{-| How long ago, in the largest unit that fits: "now", "5m", "3h", "2d", "4mo", "2y".
-}
ago : Time.Posix -> Time.Posix -> String
ago now past =
    let
        seconds =
            (Time.posixToMillis now - Time.posixToMillis past) // 1000

        minutes =
            seconds // 60

        hours =
            minutes // 60

        days =
            hours // 24
    in
    if minutes < 1 then
        "now"

    else if hours < 1 then
        String.fromInt minutes ++ "m"

    else if days < 1 then
        String.fromInt hours ++ "h"

    else if days < 30 then
        String.fromInt days ++ "d"

    else if days < 365 then
        String.fromInt (days // 30) ++ "mo"

    else
        String.fromInt (days // 365) ++ "y"


{-| 14:05.
-}
clock : Time.Zone -> Time.Posix -> String
clock zone time =
    String.padLeft 2 '0' (String.fromInt (Time.toHour zone time))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (Time.toMinute zone time))
