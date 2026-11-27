module Link exposing (Shared, toQuery, parse)

{-| What a shared link carries: the query, the filters and the order, in the URL's query
string. Defaults are left out, so a plain view is a plain URL.

    ?q=router&topic=react&language=TypeScript&user=remix-run&stars=1000-&lifecycle=active&origin=source&sort=stars

Each picked value is its own parameter, so names such as `C#` survive the encoding. Anything
a link gets wrong falls back to the default rather than failing the whole link.

-}

import Dict
import Filters exposing (Choice(..), Filters, Order(..))
import Repo exposing (Lifecycle(..), Origin(..))
import Set
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query


type alias Shared =
    { query : String
    , filters : Filters
    , order : Order
    }


{-| `""` when everything is at its default, otherwise `?` and the parameters.
-}
toQuery : Shared -> String
toQuery { query, filters, order } =
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
            , optional "sort"
                (case order of
                    Relevance ->
                        ""

                    RecentlyStarred ->
                        "starred"

                    MostStars ->
                        "stars"

                    RecentlyUpdated ->
                        "updated"
                )
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
        |> Maybe.withDefault { query = "", filters = Filters.initial, order = Relevance }


parser : Query.Parser Shared
parser =
    Query.map8
        (\q topics languages users stars lifecycle origin order ->
            { query = Maybe.withDefault "" q
            , filters =
                { lifecycle = choice lifecycle
                , origin = choice origin
                , stars = stars |> Maybe.andThen Filters.starsFromString |> Maybe.withDefault Filters.initial.stars
                , topics = Set.fromList topics
                , languages = Set.fromList languages
                , owners = Set.fromList users
                }
            , order = Maybe.withDefault Relevance order
            }
        )
        (Query.string "q")
        (Query.custom "topic" identity)
        (Query.custom "language" identity)
        (Query.custom "user" identity)
        (Query.string "stars")
        (Query.enum "lifecycle" (Dict.fromList [ ( "active", Active ), ( "archived", Archived ) ]))
        (Query.enum "origin" (Dict.fromList [ ( "source", Source ), ( "fork", Fork ) ]))
        (Query.enum "sort" (Dict.fromList [ ( "starred", RecentlyStarred ), ( "stars", MostStars ), ( "updated", RecentlyUpdated ) ]))


choice : Maybe a -> Choice a
choice =
    Maybe.map Only >> Maybe.withDefault Any
