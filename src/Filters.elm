module Filters exposing
    ( Filters, Choice(..), initial, isInitial, matches
    , Facet(..), facets, facetLabel, picked, pick, clear, valuesOf, hasPicks
    , Stars, starsFromSlider, starsToSlider, starsLabel, tierCount, isAnyStars, starsToString, starsFromString
    , Sort(..), Key(..), Direction(..), keys, keyLabel, flip
    )

{-| What narrows and orders the list, apart from the query.
-}

import Array exposing (Array)
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


{-| A range of star counts on a log scale: the low and high tiers, low never above high.
The top tier has no upper bound.
-}
type Stars
    = Stars Int Int


tiers : Array Int
tiers =
    Array.fromList [ 0, 10, 100, 1000, 10000, 100000 ]


tierCount : Int
tierCount =
    Array.length tiers


anyStars : Stars
anyStars =
    Stars 0 tierCount


isAnyStars : Stars -> Bool
isAnyStars stars =
    stars == anyStars


starsFromSlider : ( Float, Float ) -> Stars
starsFromSlider ( low, high ) =
    let
        clampTier =
            round >> clamp 0 tierCount
    in
    Stars (min (clampTier low) (clampTier high)) (max (clampTier low) (clampTier high))


starsToSlider : Stars -> ( Float, Float )
starsToSlider (Stars low high) =
    ( toFloat low, toFloat high )


{-| The range as star counts for a link: `1000-`, `10-1000`, `-100`, or nothing for any.
-}
starsToString : Stars -> String
starsToString stars =
    let
        ( low, high ) =
            span stars
    in
    if stars == anyStars then
        ""

    else
        (if low == 0 then
            ""

         else
            Maybe.withDefault "" (Maybe.map String.fromInt (tier low))
        )
            ++ "-"
            ++ Maybe.withDefault "" (Maybe.map String.fromInt (tier high))


{-| Reads what `starsToString` wrote. Counts that aren't tiers, and backwards ranges, don't
parse.
-}
starsFromString : String -> Maybe Stars
starsFromString text =
    let
        indexOf count =
            Array.toIndexedList tiers
                |> List.filter (\( _, value ) -> String.fromInt value == count)
                |> List.head
                |> Maybe.map Tuple.first
    in
    case String.split "-" text of
        [ low, high ] ->
            Maybe.map2
                (\from to ->
                    if from <= to then
                        Just (Stars from to)

                    else
                        Nothing
                )
                (if low == "" then
                    Just 0

                 else
                    indexOf low
                )
                (if high == "" then
                    Just tierCount

                 else
                    indexOf high
                )
                |> Maybe.andThen identity

        _ ->
            Nothing


tier : Int -> Maybe Int
tier index =
    Array.get index tiers


{-| The tiers a range covers. When both thumbs sit on the same stop, the range is the step
that starts there: both on 1k means 1k–10k, both at the top means 100k and up. A range never
narrows down to a single count.
-}
span : Stars -> ( Int, Int )
span (Stars low high) =
    if low < high then
        ( low, high )

    else if low < tierCount then
        ( low, low + 1 )

    else
        ( tierCount - 1, tierCount )


inStars : Stars -> Int -> Bool
inStars stars count =
    let
        ( low, high ) =
            span stars
    in
    (Maybe.withDefault 0 (tier low) <= count)
        && (case tier high of
                Just bound ->
                    count <= bound

                Nothing ->
                    True
           )


starsLabel : Stars -> String
starsLabel stars =
    let
        ( low, high ) =
            span stars
    in
    case ( tier low, tier high ) of
        ( Just 0, Nothing ) ->
            "Any"

        ( Just from, Nothing ) ->
            Format.compact from ++ "+"

        ( Just 0, Just to ) ->
            "≤ " ++ Format.compact to

        ( Just from, Just to ) ->
            Format.compact from ++ "–" ++ Format.compact to

        ( Nothing, _ ) ->
            "Any"



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
