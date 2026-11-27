module GitHub exposing
    ( Response(..), Meta, Quota, Wait(..), Problem(..)
    , profile, starred, perPage
    )

{-| The two unauthenticated REST endpoints the app uses, and what their responses mean for
the rate limit. Every response is a value, never an error, so callers have to say what
they do with each.
-}

import Dict
import Http
import Json.Decode as Decode exposing (Decoder)
import Login exposing (Login)
import Profile exposing (Profile)
import Repo exposing (Repo)
import Task exposing (Task)
import Time
import Url


base : String
base =
    "https://api.github.com"


type Response a
    = Fresh a Meta
    | NotModified Meta
    | NotFound
    | RateLimited Wait
    | Failed Problem


type alias Meta =
    { etag : Maybe String
    , lastPage : Maybe Int
    , quota : Maybe Quota
    }


{-| Requests left in the current window, and when it starts again.
-}
type alias Quota =
    { remaining : Int
    , resetsAt : Time.Posix
    }


{-| How long GitHub wants us to wait, from the most specific signal it gave.
-}
type Wait
    = RetryAfter Int
    | Until Time.Posix
    | Unspecified


type Problem
    = Offline
    | TimedOut
    | ServerError Int
    | Unreadable


perPage : Int
perPage =
    100


profile : Login -> Maybe String -> Task Never (Response (Time.Posix -> Profile))
profile login etag =
    get
        { url = base ++ "/users/" ++ Login.toString login
        , accept = "application/vnd.github+json"
        , etag = etag
        , decoder = Decode.map (\toProfile info at -> toProfile at info.etag) Profile.decoder
        }
        |> Task.map
            (\response ->
                case response of
                    Fresh toProfile info ->
                        Fresh (toProfile info) info

                    NotModified info ->
                        NotModified info

                    NotFound ->
                        NotFound

                    RateLimited delay ->
                        RateLimited delay

                    Failed problem ->
                        Failed problem
            )


{-| A page of a user's stars, newest first, each with the time it was starred.
-}
starred : Login -> { page : Int, etag : Maybe String } -> Task Never (Response (List Repo))
starred login { page, etag } =
    get
        { url = base ++ "/users/" ++ Login.toString login ++ "/starred?per_page=" ++ String.fromInt perPage ++ "&page=" ++ String.fromInt page
        , accept = "application/vnd.github.star+json"
        , etag = etag
        , decoder = Decode.list Repo.starredDecoder
        }



-- REQUESTS


get : { url : String, accept : String, etag : Maybe String, decoder : Decoder a } -> Task Never (Response a)
get { url, accept, etag, decoder } =
    Http.task
        { method = "GET"
        , headers =
            Http.header "Accept" accept
                :: Http.header "X-GitHub-Api-Version" "2022-11-28"
                :: (case etag of
                        Just value ->
                            [ Http.header "If-None-Match" value ]

                        Nothing ->
                            []
                   )
        , url = url
        , body = Http.emptyBody
        , resolver = Http.stringResolver (classify decoder >> Ok)
        , timeout = Just 20000
        }
        |> Task.onError never


classify : Decoder a -> Http.Response String -> Response a
classify decoder response =
    case response of
        Http.GoodStatus_ metadata body ->
            case Decode.decodeString decoder body of
                Ok value ->
                    Fresh value (meta metadata)

                Err _ ->
                    Failed Unreadable

        Http.BadStatus_ metadata _ ->
            let
                info =
                    meta metadata
            in
            if metadata.statusCode == 304 then
                NotModified info

            else if metadata.statusCode == 404 then
                NotFound

            else if metadata.statusCode == 429 || (metadata.statusCode == 403 && isRateLimit metadata info) then
                RateLimited (wait metadata info)

            else
                Failed (ServerError metadata.statusCode)

        Http.NetworkError_ ->
            Failed Offline

        Http.Timeout_ ->
            Failed TimedOut

        Http.BadUrl_ _ ->
            Failed Unreadable


{-| An unauthenticated 403 is a primary limit when no requests are left, and a secondary one
when GitHub says how long to back off.
-}
isRateLimit : Http.Metadata -> Meta -> Bool
isRateLimit metadata info =
    Maybe.map (.remaining >> (==) 0) info.quota == Just True || header "retry-after" metadata /= Nothing


{-| GitHub's order of precedence: `retry-after`, then the reset of an exhausted window, then
"at least a minute".
-}
wait : Http.Metadata -> Meta -> Wait
wait metadata info =
    case header "retry-after" metadata |> Maybe.andThen String.toInt of
        Just seconds ->
            RetryAfter seconds

        Nothing ->
            case info.quota of
                Just quota ->
                    if quota.remaining == 0 then
                        Until quota.resetsAt

                    else
                        Unspecified

                Nothing ->
                    Unspecified


meta : Http.Metadata -> Meta
meta metadata =
    { etag = header "etag" metadata
    , lastPage = header "link" metadata |> Maybe.andThen lastPage
    , quota =
        Maybe.map2 Quota
            (header "x-ratelimit-remaining" metadata |> Maybe.andThen String.toInt)
            (header "x-ratelimit-reset" metadata |> Maybe.andThen String.toInt |> Maybe.map (\seconds -> Time.millisToPosix (seconds * 1000)))
    }


header : String -> Http.Metadata -> Maybe String
header name metadata =
    Dict.foldl
        (\key value found ->
            if String.toLower key == name then
                Just value

            else
                found
        )
        Nothing
        metadata.headers


{-| The `page` of the `rel="last"` link:

    <https://api.github.com/user/1/starred?per_page=100&page=21>; rel="last"

-}
lastPage : String -> Maybe Int
lastPage link =
    String.split "," link
        |> List.filter (String.contains "rel=\"last\"")
        |> List.head
        |> Maybe.andThen (String.split ";" >> List.head)
        |> Maybe.map (String.trim >> String.replace "<" "" >> String.replace ">" "")
        |> Maybe.andThen Url.fromString
        |> Maybe.andThen .query
        |> Maybe.andThen
            (String.split "&"
                >> List.filterMap
                    (\pair ->
                        case String.split "=" pair of
                            [ "page", value ] ->
                                String.toInt value

                            _ ->
                                Nothing
                    )
                >> List.head
            )
