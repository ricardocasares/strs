module Link exposing (Shared, toQuery, parse)

{-| What a shared link carries: the query, the filters and the order, in the URL's query
string. Defaults are left out, so a plain view is a plain URL.

    ?q=router&topic=react&language=TypeScript&user=remix-run&stars=1000-&lifecycle=active&origin=source&sort=stars-asc

Each picked value is its own parameter, so names such as `C#` survive the encoding. Anything
a link gets wrong falls back to the default rather than failing the whole link.

-}

import Dict
import Filters exposing (Choice(..), Direction(..), Filters, Key(..), Sort(..))
import Repo exposing (Lifecycle(..), Origin(..))
import Set
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query


type alias Shared =
    { query : String
    , filters : Filters
    , sort : Sort
    }


{-| `""` when everything is at its default, otherwise `?` and the parameters.
-}
toQuery : Shared -> String
toQuery { query, filters, sort } =
    Builder.toQuery
        (List.concat
            [ optional "q" (String.trim query)
            , List.map (Builder.string "topic") (Set.toList filters.topics)
            , List.map (Builder.string "language") (Set.toList filters.languages)
            , List.map (Builder.string "user") (Set.toList filters.owners)
            , optional "stars" (Filters.starsToString filters.stars)
            , optional "lifecycle"
                (case filters.lifecycle of
                    Any ->
                        ""

                    Only Active ->
                        "active"

                    Only Archived ->
                        "archived"
                )
            , optional "origin"
                (case filters.origin of
                    Any ->
                        ""

                    Only Source ->
                        "source"

                    Only Fork ->
                        "fork"
                )
            , optional "sort" (sortToString sort)
            ]
        )


optional : String -> String -> List Builder.QueryParameter
optional key value =
    if String.isEmpty value then
        []

    else
        [ Builder.string key value ]


parse : Url -> Shared
parse url =
    Parser.parse (Parser.top <?> parser) { url | path = "/" }
        |> Maybe.withDefault { query = "", filters = Filters.initial, sort = Relevance }


parser : Query.Parser Shared
parser =
    Query.map8
        (\q topics languages users stars lifecycle origin sort ->
            { query = Maybe.withDefault "" q
            , filters =
                { lifecycle = choice lifecycle
                , origin = choice origin
                , stars = stars |> Maybe.andThen Filters.starsFromString |> Maybe.withDefault Filters.initial.stars
                , topics = Set.fromList topics
                , languages = Set.fromList languages
                , owners = Set.fromList users
                }
            , sort = Maybe.withDefault Relevance sort
            }
        )
        (Query.string "q")
        (Query.custom "topic" identity)
        (Query.custom "language" identity)
        (Query.custom "user" identity)
        (Query.string "stars")
        (Query.enum "lifecycle" (Dict.fromList [ ( "active", Active ), ( "archived", Archived ) ]))
        (Query.enum "origin" (Dict.fromList [ ( "source", Source ), ( "fork", Fork ) ]))
        (Query.enum "sort"
            (Dict.fromList
                (List.concatMap
                    (\key -> List.map (\direction -> ( sortToString (Sorted key direction), Sorted key direction )) [ Descending, Ascending ])
                    Filters.keys
                )
            )
        )


{-| `stars` for most first, `stars-asc` for fewest first; nothing for best match.
-}
sortToString : Sort -> String
sortToString sort =
    case sort of
        Relevance ->
            ""

        Sorted key direction ->
            (case key of
                StarredAt ->
                    "starred"

                StarCount ->
                    "stars"

                PushedAt ->
                    "updated"
            )
                ++ (case direction of
                        Descending ->
                            ""

                        Ascending ->
                            "-asc"
                   )


choice : Maybe a -> Choice a
choice =
    Maybe.map Only >> Maybe.withDefault Any
