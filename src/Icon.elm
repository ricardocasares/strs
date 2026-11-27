module Icon exposing
    ( Icon, view
    , search, close, star, fork, archive, refresh, sun, moon, monitor
    , chevronsUpDown, sort, external, userSearch, userX, searchX, reset, sparkles
    )

{-| Lucide icons (ISC licence), drawn inline so they follow `currentColor` and font size.
-}

import Html exposing (Html)
import Svg exposing (Svg)
import Svg.Attributes as A


type Icon
    = Icon (List (Svg Never))


{-| The icon, with classes after its own `size-4 shrink-0`.
-}
view : String -> Icon -> Html msg
view classes (Icon shapes) =
    Svg.svg
        [ A.viewBox "0 0 24 24"
        , A.fill "none"
        , A.stroke "currentColor"
        , A.strokeWidth "2"
        , A.strokeLinecap "round"
        , A.strokeLinejoin "round"
        , A.class ("size-4 shrink-0 " ++ classes)
        ]
        (List.map (Svg.map never) shapes)


path : String -> Svg Never
path d =
    Svg.path [ A.d d ] []


circle : String -> String -> String -> Svg Never
circle cx cy r =
    Svg.circle [ A.cx cx, A.cy cy, A.r r ] []


rect : { x : String, y : String, w : String, h : String, r : String } -> Svg Never
rect { x, y, w, h, r } =
    Svg.rect [ A.x x, A.y y, A.width w, A.height h, A.rx r ] []


search : Icon
search =
    Icon [ circle "11" "11" "8", path "m21 21-4.3-4.3" ]


close : Icon
close =
    Icon [ path "M18 6 6 18", path "m6 6 12 12" ]


star : Icon
star =
    Icon [ path "M11.525 2.295a.53.53 0 0 1 .95 0l2.31 4.679a2.123 2.123 0 0 0 1.595 1.16l5.166.756a.53.53 0 0 1 .294.904l-3.736 3.638a2.123 2.123 0 0 0-.611 1.878l.882 5.14a.53.53 0 0 1-.771.56l-4.618-2.428a2.122 2.122 0 0 0-1.973 0L6.396 21.01a.53.53 0 0 1-.77-.56l.881-5.139a2.122 2.122 0 0 0-.611-1.879L2.16 9.795a.53.53 0 0 1 .294-.906l5.165-.755a2.122 2.122 0 0 0 1.597-1.16z" ]


fork : Icon
fork =
    Icon [ circle "12" "18" "3", circle "6" "6" "3", circle "18" "6" "3", path "M18 9v2c0 .6-.4 1-1 1H7c-.6 0-1-.4-1-1V9", path "M12 12v3" ]


archive : Icon
archive =
    Icon [ rect { x = "2", y = "3", w = "20", h = "5", r = "1" }, path "M4 8v11a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8", path "M10 12h4" ]


refresh : Icon
refresh =
    Icon [ path "M3 12a9 9 0 0 1 9-9 9.75 9.75 0 0 1 6.74 2.74L21 8", path "M21 3v5h-5", path "M21 12a9 9 0 0 1-9 9 9.75 9.75 0 0 1-6.74-2.74L3 16", path "M8 16H3v5" ]


sun : Icon
sun =
    Icon [ circle "12" "12" "4", path "M12 2v2", path "M12 20v2", path "m4.93 4.93 1.41 1.41", path "m17.66 17.66 1.41 1.41", path "M2 12h2", path "M20 12h2", path "m6.34 17.66-1.41 1.41", path "m19.07 4.93-1.41 1.41" ]


moon : Icon
moon =
    Icon [ path "M12 3a6 6 0 0 0 9 9 9 9 0 1 1-9-9Z" ]


monitor : Icon
monitor =
    Icon [ rect { x = "2", y = "3", w = "20", h = "14", r = "2" }, path "M8 21h8", path "M12 17v4" ]


chevronsUpDown : Icon
chevronsUpDown =
    Icon [ path "m7 15 5 5 5-5", path "m7 9 5-5 5 5" ]


sort : Icon
sort =
    Icon [ path "m3 16 4 4 4-4", path "M7 20V4", path "m21 8-4-4-4 4", path "M17 4v16" ]


external : Icon
external =
    Icon [ path "M15 3h6v6", path "M10 14 21 3", path "M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6" ]


userSearch : Icon
userSearch =
    Icon [ circle "10" "7" "4", path "M10.3 15H7a4 4 0 0 0-4 4v2", circle "17" "17" "3", path "m21 21-1.9-1.9" ]


userX : Icon
userX =
    Icon [ path "M16 21v-2a4 4 0 0 0-4-4H6a4 4 0 0 0-4 4v2", circle "9" "7" "4", path "m17 8 5 5", path "m22 8-5 5" ]


searchX : Icon
searchX =
    Icon [ path "m13.5 8.5-5 5", path "m8.5 8.5 5 5", circle "11" "11" "8", path "m21 21-4.3-4.3" ]


reset : Icon
reset =
    Icon [ path "M3 12a9 9 0 1 0 9-9 9.75 9.75 0 0 0-6.74 2.74L3 8", path "M3 3v5h5" ]


sparkles : Icon
sparkles =
    Icon [ path "M9.937 15.5A2 2 0 0 0 8.5 14.063l-6.135-1.582a.5.5 0 0 1 0-.962L8.5 9.936A2 2 0 0 0 9.937 8.5l1.582-6.135a.5.5 0 0 1 .963 0L14.063 8.5A2 2 0 0 0 15.5 9.937l6.135 1.581a.5.5 0 0 1 0 .964L15.5 14.063a2 2 0 0 0-1.437 1.437l-1.582 6.135a.5.5 0 0 1-.963 0z" ]
