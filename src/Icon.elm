module Icon exposing
    ( Icon, view
    , search, close, star, fork, archive, refresh, sun, moon
    , monitor, chevronsUpDown, caretDown, caretUp, sort, userX, searchX, clock
    , hash, code, users, descending, ascending, fullResync
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


{-| Largest first: long bars on top, an arrow down. Phosphor names its glyphs the other way
round, so these are picked by the picture, not the name.
-}
descending : Icon
descending =
    Phosphor.sortAscending


{-| Smallest first: short bars on top, an arrow up.
-}
ascending : Icon
ascending =
    Phosphor.sortDescending


fullResync : Icon
fullResync =
    Phosphor.cloudCheck
