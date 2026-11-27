module Icon exposing
    ( Icon, view
    , search, close, star, fork, archive, refresh, sun, moon
    , monitor, chevronsUpDown, caretDown, caretUp, sort, userX, searchX, clock
    , hash, code, users, wideFirst, narrowFirst, shootingStar, fullResync
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


sort : Icon
sort =
    Phosphor.arrowsDownUp


userX : Icon
userX =
    Phosphor.userCircleMinus


searchX : Icon
searchX =
    Phosphor.magnifyingGlassMinus


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


{-| Long bars on top, narrowing down. Phosphor names its sort glyphs the other way round, so
these are named by their picture.
-}
wideFirst : Icon
wideFirst =
    Phosphor.sortAscending


{-| Short bars on top, widening down.
-}
narrowFirst : Icon
narrowFirst =
    Phosphor.sortDescending


fullResync : Icon
fullResync =
    Phosphor.cloudCheck


shootingStar : Icon
shootingStar =
    Phosphor.shootingStar
