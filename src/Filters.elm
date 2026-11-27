module Filters exposing
    ( Filters, Choice(..), initial, isInitial, matches
    , Facet(..), facets, facetLabel, picked, pick, clear, valuesOf, hasPicks
    , Stars, anyStars, isAnyStars, stops, starsFromSlider, starsToSlider, starsLabel, starsToString, starsFromString
    , Sort(..), Key(..), Direction(..), keys, keyLabel, flip
    )

{-| What narrows and orders the list, apart from the query.
-}

import Format
import Repo exposing (Lifecycle, Origin, Repo)
import Set exposing (Set)


type alias Filters =
    { lifecycle : Choice Lifecycle
    , origin : Choice Origin
    , stars : Stars
    , languages : Set String
    , topics : Set String
    , owners : Set String
    }


{-| Either side of a two-sided property, or both.
-}
type Choice a
    = Any
    | Only a


initial : Filters
initial =
    { lifecycle = Any
    , origin = Any
    , stars = anyStars
    , languages = Set.empty
    , topics = Set.empty
    , owners = Set.empty
    }


isInitial : Filters -> Bool
isInitial filters =
    filters == initial


{-| Everything but the facets, which are counted over the repos that pass the rest.
-}
matches : Filters -> Repo -> Bool
matches filters repo =
    choose filters.lifecycle repo.lifecycle
        && choose filters.origin repo.origin
        && inStars filters.stars repo.stars


choose : Choice a -> a -> Bool
choose choice value =
    case choice of
        Any ->
            True

        Only wanted ->
            wanted == value



-- FACETS


{-| A property repos are counted by in the sidebar, and narrowed to by picking values. Picking
several values of one facet widens to any of them; picking in several facets narrows to all.
-}
type Facet
    = Language
    | Topic
    | Owner


{-| Every facet, in the order the sidebar offers them. Built through a `case`, so a new facet
can't be left out.
-}
facets : List Facet
facets =
    let
        next list =
            case list of
                [] ->
                    next [ Topic ]

                Topic :: _ ->
                    next (Language :: list)

                Language :: _ ->
                    next (Owner :: list)

                Owner :: _ ->
                    list
    in
    List.reverse (next [])


facetLabel : Facet -> String
facetLabel facet =
    case facet of
        Language ->
            "Languages"

        Topic ->
            "Topics"

        Owner ->
            "Users"


picked : Facet -> Filters -> Set String
picked facet filters =
    case facet of
        Language ->
            filters.languages

        Topic ->
            filters.topics

        Owner ->
            filters.owners


setPicked : Facet -> Set String -> Filters -> Filters
setPicked facet values filters =
    case facet of
        Language ->
            { filters | languages = values }

        Topic ->
            { filters | topics = values }

        Owner ->
            { filters | owners = values }


{-| Picks a value, or unpicks it when it's picked already.
-}
pick : Facet -> String -> Filters -> Filters
pick facet value filters =
    let
        current =
            picked facet filters
    in
    setPicked facet
        (if Set.member value current then
            Set.remove value current

         else
            Set.insert value current
        )
        filters


clear : Facet -> Filters -> Filters
clear facet =
    setPicked facet Set.empty


hasPicks : Filters -> Bool
hasPicks filters =
    List.any (\facet -> not (Set.isEmpty (picked facet filters))) facets


{-| A repo's values for a facet: its language, its topics, its owner.
-}
valuesOf : Facet -> Repo -> List String
valuesOf facet repo =
    case facet of
        Language ->
            Maybe.withDefault [] (Maybe.map List.singleton repo.language)

        Topic ->
            repo.topics

        Owner ->
            [ repo.owner ]



-- STARS


{-| A range of star counts, either end open. Open ends follow the library: an open low end
starts at its fewest stars, an open high end goes up to its most. Low is never above high.
-}
type Stars
    = Stars (Maybe Int) (Maybe Int)


anyStars : Stars
anyStars =
    Stars Nothing Nothing


isAnyStars : Stars -> Bool
isAnyStars stars =
    stars == anyStars


inStars : Stars -> Int -> Bool
inStars (Stars low high) count =
    Maybe.withDefault True (Maybe.map (\bound -> bound <= count) low)
        && Maybe.withDefault True (Maybe.map (\bound -> count <= bound) high)


{-| The slider's stops for a library with these fewest and most stars: its two ends, and the
powers of ten in between, so the scale is logarithmic and never leaves the real counts.
-}
stops : ( Int, Int ) -> List Int
stops ( fewest, most ) =
    fewest
        :: List.filter (\step -> fewest < step && step < most) [ 10, 100, 1000, 10000, 100000, 1000000 ]
        ++ (if most > fewest then
                [ most ]

            else
                []
           )


{-| Where the thumbs sit for a range: an open end at its end of the scale, a bound at the
first stop that reaches it.
-}
starsToSlider : List Int -> Stars -> ( Float, Float )
starsToSlider scale (Stars low high) =
    let
        last =
            List.length scale - 1

        position bound =
            scale
                |> List.indexedMap Tuple.pair
                |> List.filter (\( _, step ) -> step >= bound)
                |> List.head
                |> Maybe.map Tuple.first
                |> Maybe.withDefault last
    in
    ( Maybe.map position low |> Maybe.withDefault 0 |> toFloat
    , Maybe.map position high |> Maybe.withDefault last |> toFloat
    )


{-| The range the thumbs pick. A thumb on an end of the scale leaves that end open, so the
range keeps following the library as it grows.
-}
starsFromSlider : List Int -> ( Float, Float ) -> Stars
starsFromSlider scale ( low, high ) =
    let
        last =
            List.length scale - 1

        at index =
            List.drop index scale |> List.head

        lowIndex =
            clamp 0 last (round (min low high))

        highIndex =
            clamp 0 last (round (max low high))
    in
    Stars
        (if lowIndex == 0 then
            if highIndex == 0 then
                -- Both thumbs on the fewest stars: exactly that many.
                at 0

            else
                Nothing

         else
            at lowIndex
        )
        (if highIndex == last then
            if lowIndex == last then
                at last

            else
                Nothing

         else
            at highIndex
        )


starsLabel : Stars -> String
starsLabel (Stars low high) =
    case ( low, high ) of
        ( Nothing, Nothing ) ->
            "Any"

        ( Just from, Nothing ) ->
            Format.compact from ++ "+"

        ( Nothing, Just to ) ->
            "≤ " ++ Format.compact to

        ( Just from, Just to ) ->
            if from == to then
                Format.compact from

            else
                Format.compact from ++ "–" ++ Format.compact to


{-| The range for a link: `1000-`, `10-1000`, `-100`, or nothing for any.
-}
starsToString : Stars -> String
starsToString (Stars low high) =
    if low == Nothing && high == Nothing then
        ""

    else
        Maybe.withDefault "" (Maybe.map String.fromInt low)
            ++ "-"
            ++ Maybe.withDefault "" (Maybe.map String.fromInt high)


{-| Reads what `starsToString` wrote. Backwards ranges don't parse.
-}
starsFromString : String -> Maybe Stars
starsFromString text =
    let
        bound part =
            if part == "" then
                Just Nothing

            else
                Maybe.map Just (String.toInt part)
    in
    case String.split "-" text of
        [ low, high ] ->
            Maybe.map2 Tuple.pair (bound low) (bound high)
                |> Maybe.andThen
                    (\( from, to ) ->
                        case ( from, to ) of
                            ( Just a, Just b ) ->
                                if a <= b then
                                    Just (Stars from to)

                                else
                                    Nothing

                            _ ->
                                Just (Stars from to)
                    )

        _ ->
            Nothing



-- SORT


{-| Best match first, or by an attribute in either direction. Relevance has no direction:
least relevant first is never what anyone wants.
-}
type Sort
    = Relevance
    | Sorted Key Direction


type Key
    = StarredAt
    | StarCount
    | PushedAt


type Direction
    = Descending
    | Ascending


{-| Every key, in the order the sidebar offers them.
-}
keys : List Key
keys =
    let
        next list =
            case list of
                [] ->
                    next [ StarredAt ]

                StarredAt :: _ ->
                    next (StarCount :: list)

                StarCount :: _ ->
                    next (PushedAt :: list)

                PushedAt :: _ ->
                    list
    in
    List.reverse (next [])


keyLabel : Key -> String
keyLabel key =
    case key of
        StarredAt ->
            "Date starred"

        StarCount ->
            "Stargazers"

        PushedAt ->
            "Last updated"


flip : Direction -> Direction
flip direction =
    case direction of
        Descending ->
            Ascending

        Ascending ->
            Descending
