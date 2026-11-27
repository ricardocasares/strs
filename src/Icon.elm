module Icon exposing
    ( Icon, view
    , search, close, star, fork, archive, refresh, sun, moon, monitor
    , chevronsUpDown, caretRight, caretDown, caretUp, sort, external, userSearch, userX, searchX, reset, clock
    , hash, code, users
    , relevance, starred, popular, updated
    )

{-| The app's Phosphor icons, named for what they mean here.
-}

import Html exposing (Html)
import Phosphor


type alias Icon =
    Phosphor.Icon


{-| The icon at the regular weight, sized `size-4` unless the classes say otherwise.
-}
view : String -> Icon -> Html msg
view classes icon =
    icon Phosphor.Regular
        |> Phosphor.withClass ("size-4 shrink-0 " ++ classes)
        |> Phosphor.toHtml []


search : Icon
search =
    Phosphor.magnifyingGlass


close : Icon
close =
    Phosphor.x


star : Icon
star =
    Phosphor.star


fork : Icon
fork =
    Phosphor.gitFork


archive : Icon
archive =
    Phosphor.archive


refresh : Icon
refresh =
    Phosphor.arrowsClockwise


sun : Icon
sun =
    Phosphor.sun


moon : Icon
moon =
    Phosphor.moon


monitor : Icon
monitor =
    Phosphor.monitor


chevronsUpDown : Icon
chevronsUpDown =
    Phosphor.caretUpDown


caretRight : Icon
caretRight =
    Phosphor.caretRight


sort : Icon
sort =
    Phosphor.arrowsDownUp


external : Icon
external =
    Phosphor.arrowSquareOut


userSearch : Icon
userSearch =
    Phosphor.userFocus


userX : Icon
userX =
    Phosphor.userCircleMinus


searchX : Icon
searchX =
    Phosphor.magnifyingGlassMinus


reset : Icon
reset =
    Phosphor.arrowCounterClockwise


clock : Icon
clock =
    Phosphor.clock



hash : Icon
hash =
    Phosphor.hash




caretDown : Icon
caretDown =
    Phosphor.caretDown


caretUp : Icon
caretUp =
    Phosphor.caretUp


code : Icon
code =
    Phosphor.code


users : Icon
users =
    Phosphor.users



relevance : Icon
relevance =
    Phosphor.sparkle


starred : Icon
starred =
    Phosphor.calendarStar


popular : Icon
popular =
    Phosphor.trendUp


updated : Icon
updated =
    Phosphor.gitCommit
