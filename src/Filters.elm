module Filters exposing
    ( Filters, Choice(..), initial, isInitial, matches
    , Stars, starsFromSlider, starsToSlider, starsLabel, tierCount, isAnyStars
    , Order(..), orders, orderLabel
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
    }


isInitial : Filters -> Bool
isInitial filters =
    filters == initial


{-| Everything but the languages, which are counted over the repos that pass the rest.
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


tier : Int -> Maybe Int
tier index =
    Array.get index tiers


inStars : Stars -> Int -> Bool
inStars (Stars low high) count =
    (Maybe.withDefault 0 (tier low) <= count)
        && (case tier high of
                Just bound ->
                    count <= bound

                Nothing ->
                    True
           )


starsLabel : Stars -> String
starsLabel (Stars low high) =
    case ( tier low, tier high ) of
        ( Just 0, Nothing ) ->
            "Any"

        ( Just from, Nothing ) ->
            Format.compact from ++ "+"

        ( Just 0, Just to ) ->
            "≤ " ++ Format.compact to

        ( Just from, Just to ) ->
            if from == to then
                Format.compact from

            else
                Format.compact from ++ "–" ++ Format.compact to

        ( Nothing, _ ) ->
            "Any"



-- ORDER


type Order
    = Relevance
    | RecentlyStarred
    | MostStars
    | RecentlyUpdated


orders : List Order
orders =
    [ Relevance, RecentlyStarred, MostStars, RecentlyUpdated ]


orderLabel : Order -> String
orderLabel order =
    case order of
        Relevance ->
            "Best match"

        RecentlyStarred ->
            "Recently starred"

        MostStars ->
            "Most stars"

        RecentlyUpdated ->
            "Recently updated"
