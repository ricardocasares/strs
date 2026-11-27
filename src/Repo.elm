module Repo exposing
    ( Repo, Lifecycle(..), Origin(..)
    , starredDecoder, storedDecoder, encode
    , fullName, url
    )

{-| A starred repository, slimmed down from GitHub's payload to what the app shows,
filters and searches.
-}

import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time


type alias Repo =
    { id : Int
    , owner : String
    , name : String
    , avatar : String
    , description : Maybe String
    , language : Maybe String
    , topics : List String
    , stars : Int
    , forks : Int
    , lifecycle : Lifecycle
    , origin : Origin
    , pushedAt : Time.Posix
    , starredAt : Time.Posix
    }


type Lifecycle
    = Active
    | Archived


type Origin
    = Source
    | Fork


fullName : Repo -> String
fullName repo =
    repo.owner ++ "/" ++ repo.name


url : Repo -> String
url repo =
    "https://github.com/" ++ fullName repo



-- GITHUB


{-| One item of `/users/{login}/starred` with the `star+json` media type:
`{ starred_at, repo }`.
-}
starredDecoder : Decoder Repo
starredDecoder =
    Decode.map2 (\starredAt toRepo -> toRepo starredAt)
        (Decode.field "starred_at" Iso8601.decoder)
        (Decode.field "repo" githubDecoder)


githubDecoder : Decoder (Time.Posix -> Repo)
githubDecoder =
    Decode.succeed Repo
        |> field "id" Decode.int
        |> field "owner" (Decode.field "login" Decode.string)
        |> field "name" Decode.string
        |> field "owner" (Decode.field "avatar_url" Decode.string)
        |> field "description" (Decode.nullable Decode.string |> Decode.map (Maybe.andThen nonBlank))
        |> field "language" (Decode.nullable Decode.string)
        |> optional "topics" (Decode.list Decode.string) []
        |> field "stargazers_count" Decode.int
        |> field "forks_count" Decode.int
        |> field "archived" (lifecycle Decode.bool)
        |> field "fork" (origin Decode.bool)
        |> optional "pushed_at" Iso8601.decoder (Time.millisToPosix 0)


nonBlank : String -> Maybe String
nonBlank text =
    if String.isEmpty (String.trim text) then
        Nothing

    else
        Just text


lifecycle : Decoder Bool -> Decoder Lifecycle
lifecycle =
    Decode.map
        (\archived ->
            if archived then
                Archived

            else
                Active
        )


origin : Decoder Bool -> Decoder Origin
origin =
    Decode.map
        (\fork ->
            if fork then
                Fork

            else
                Source
        )



-- STORAGE


storedDecoder : Decoder Repo
storedDecoder =
    Decode.succeed Repo
        |> field "id" Decode.int
        |> field "owner" Decode.string
        |> field "name" Decode.string
        |> field "avatar" Decode.string
        |> field "description" (Decode.nullable Decode.string)
        |> field "language" (Decode.nullable Decode.string)
        |> field "topics" (Decode.list Decode.string)
        |> field "stars" Decode.int
        |> field "forks" Decode.int
        |> field "archived" (lifecycle Decode.bool)
        |> field "fork" (origin Decode.bool)
        |> field "pushedAt" posix
        |> field "starredAt" posix


{-| The stored record. `login` and `id` are its key; `seen` is the full sync that last saw
it, so the stars a later full sync doesn't see again can be pruned.
-}
encode : { login : String, seen : Time.Posix } -> Repo -> Encode.Value
encode { login, seen } repo =
    Encode.object
        [ ( "login", Encode.string login )
        , ( "seen", Encode.int (Time.posixToMillis seen) )
        , ( "id", Encode.int repo.id )
        , ( "owner", Encode.string repo.owner )
        , ( "name", Encode.string repo.name )
        , ( "avatar", Encode.string repo.avatar )
        , ( "description", maybe Encode.string repo.description )
        , ( "language", maybe Encode.string repo.language )
        , ( "topics", Encode.list Encode.string repo.topics )
        , ( "stars", Encode.int repo.stars )
        , ( "forks", Encode.int repo.forks )
        , ( "archived", Encode.bool (repo.lifecycle == Archived) )
        , ( "fork", Encode.bool (repo.origin == Fork) )
        , ( "pushedAt", Encode.int (Time.posixToMillis repo.pushedAt) )
        , ( "starredAt", Encode.int (Time.posixToMillis repo.starredAt) )
        ]



-- HELPERS


field : String -> Decoder a -> Decoder (a -> b) -> Decoder b
field name decoder =
    Decode.map2 (|>) (Decode.field name decoder)


optional : String -> Decoder a -> a -> Decoder (a -> b) -> Decoder b
optional name decoder fallback =
    Decode.map2 (|>)
        (Decode.oneOf [ Decode.field name (Decode.nullable decoder) |> Decode.map (Maybe.withDefault fallback), Decode.succeed fallback ])


posix : Decoder Time.Posix
posix =
    Decode.map Time.millisToPosix Decode.int


maybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
maybe encoder =
    Maybe.map encoder >> Maybe.withDefault Encode.null
