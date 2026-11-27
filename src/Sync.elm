module Sync exposing
    ( Cursor(..), Synced, Run
    , Mode(..), Job, plan, request, progress, pages
    , Step(..), Save, Finish(..), advance, unchanged
    , cursorDecoder, encodeCursor, syncedAt
    )

{-| Incremental syncing of a user's stars, as pure functions over values.

GitHub lists stars newest first. A full sync walks every page and then prunes what it didn't
see again: stars that were removed. A delta sync walks from the first page until it reaches
a star it already has, so most visits cost one request.

The cursor is saved with every page, in the same write, so a sync that is interrupted, by a
rate limit or a closed tab, resumes where it stopped.

-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Repo exposing (Repo)
import Time


type Cursor
    = Fresh
    | Partial Run
    | Complete Synced


{-| A full sync in progress. `run` is when it started, and marks every star it sees.
`head` is the newest star, from the first page, which becomes the cursor once it's done.
-}
type alias Run =
    { run : Time.Posix
    , next : Int
    , last : Maybe Int
    , head : Maybe Time.Posix
    , etag : Maybe String
    }


type alias Synced =
    { newest : Maybe Time.Posix
    , etag : Maybe String
    , syncedAt : Time.Posix
    , fullAt : Time.Posix
    }


type Mode
    = Refresh
    | Rebuild


type Job
    = Full Run
    | Delta
        { synced : Synced
        , page : Int
        , last : Maybe Int
        , etag : Maybe String
        , head : Maybe Time.Posix
        , added : Int
        }


{-| A full sync is due when nothing was synced yet, when one was interrupted, when it's
asked for, or once a week, to refresh star counts and drop unstarred repos.
-}
plan : Mode -> Time.Posix -> Cursor -> Job
plan mode now cursor =
    case ( mode, cursor ) of
        ( _, Partial run ) ->
            Full run

        ( Refresh, Complete synced ) ->
            if Time.posixToMillis now - Time.posixToMillis synced.fullAt < week then
                Delta { synced = synced, page = 1, last = Nothing, etag = synced.etag, head = Nothing, added = 0 }

            else
                fullFrom now

        _ ->
            fullFrom now


fullFrom : Time.Posix -> Job
fullFrom now =
    Full { run = now, next = 1, last = Nothing, head = Nothing, etag = Nothing }


week : Int
week =
    7 * 24 * 60 * 60 * 1000


{-| The page to fetch next. Only the first page of a delta is conditional: when it hasn't
changed, nothing has.
-}
request : Job -> { page : Int, etag : Maybe String }
request job =
    case job of
        Full run ->
            { page = run.next, etag = Nothing }

        Delta delta ->
            { page = delta.page
            , etag =
                if delta.page == 1 then
                    delta.synced.etag

                else
                    Nothing
            }


{-| How far through its pages the job is, when it knows how many there are.
-}
progress : Job -> Maybe Float
progress job =
    pages job
        |> Maybe.map (\{ done, last } -> toFloat done / toFloat (max 1 last))


{-| How many pages are stored, out of how many, once GitHub has said how many there are.
-}
pages : Job -> Maybe { done : Int, last : Int }
pages job =
    case job of
        Full run ->
            Maybe.map (\last -> { done = run.next - 1, last = last }) run.last

        Delta delta ->
            Maybe.map (\last -> { done = delta.page - 1, last = last }) delta.last



-- STEPS


{-| The repos of a page to store, the full sync that saw them, and the cursor to store with
them.
-}
type alias Save =
    { repos : List Repo
    , seen : Time.Posix
    , cursor : Cursor
    }


type Step
    = Continue Job Save
    | Done Finish Save


type Finish
    = Caught Int
    | Rebuilt { prune : Time.Posix }


advance : Time.Posix -> { repos : List Repo, lastPage : Maybe Int, etag : Maybe String } -> Job -> Step
advance now page job =
    let
        isLast current last =
            List.length page.repos < 100 || Maybe.withDefault current last <= current

        newestOnPage =
            List.head page.repos |> Maybe.map .starredAt
    in
    case job of
        Full run ->
            let
                started =
                    if run.next == 1 then
                        { run | last = page.lastPage, head = newestOnPage, etag = page.etag }

                    else
                        run
            in
            if isLast run.next page.lastPage then
                Done (Rebuilt { prune = run.run })
                    { repos = page.repos
                    , seen = run.run
                    , cursor = Complete { newest = started.head, etag = started.etag, syncedAt = now, fullAt = run.run }
                    }

            else
                Continue (Full { started | next = run.next + 1 })
                    { repos = page.repos
                    , seen = run.run
                    , cursor = Partial { started | next = run.next + 1 }
                    }

        Delta delta ->
            let
                isNew repo =
                    case delta.synced.newest of
                        Just newest ->
                            Time.posixToMillis repo.starredAt > Time.posixToMillis newest

                        Nothing ->
                            True

                fresh =
                    List.length (List.filter isNew page.repos)

                started =
                    if delta.page == 1 then
                        { delta | last = page.lastPage, head = newestOnPage, etag = page.etag }

                    else
                        delta

                added =
                    delta.added + fresh

                save cursor =
                    { repos = page.repos, seen = delta.synced.fullAt, cursor = cursor }
            in
            if fresh < List.length page.repos || isLast delta.page page.lastPage then
                Done (Caught added)
                    (save
                        (Complete
                            { newest = oneOf started.head delta.synced.newest
                            , etag = started.etag
                            , syncedAt = now
                            , fullAt = delta.synced.fullAt
                            }
                        )
                    )

            else
                Continue (Delta { started | page = delta.page + 1, added = added })
                    (save (Complete delta.synced))


{-| The first page answered 304: nothing changed since the last sync.
-}
unchanged : Time.Posix -> Job -> Maybe Step
unchanged now job =
    case job of
        Delta delta ->
            Just
                (Done (Caught 0)
                    { repos = []
                    , seen = delta.synced.fullAt
                    , cursor = Complete { newest = delta.synced.newest, etag = delta.synced.etag, syncedAt = now, fullAt = delta.synced.fullAt }
                    }
                )

        Full _ ->
            Nothing


oneOf : Maybe a -> Maybe a -> Maybe a
oneOf first second =
    case first of
        Just _ ->
            first

        Nothing ->
            second


syncedAt : Cursor -> Maybe Time.Posix
syncedAt cursor =
    case cursor of
        Complete synced ->
            Just synced.syncedAt

        _ ->
            Nothing



-- STORAGE


cursorDecoder : Decoder Cursor
cursorDecoder =
    Decode.field "tag" Decode.string
        |> Decode.andThen
            (\tag ->
                case tag of
                    "Fresh" ->
                        Decode.succeed Fresh

                    "Partial" ->
                        Decode.map5 Run
                            (Decode.field "run" posix)
                            (Decode.field "next" Decode.int)
                            (Decode.field "last" (Decode.nullable Decode.int))
                            (Decode.field "head" (Decode.nullable posix))
                            (Decode.field "etag" (Decode.nullable Decode.string))
                            |> Decode.map Partial

                    "Complete" ->
                        Decode.map4 Synced
                            (Decode.field "newest" (Decode.nullable posix))
                            (Decode.field "etag" (Decode.nullable Decode.string))
                            (Decode.field "syncedAt" posix)
                            (Decode.field "fullAt" posix)
                            |> Decode.map Complete

                    _ ->
                        Decode.succeed Fresh
            )


encodeCursor : Cursor -> Encode.Value
encodeCursor cursor =
    case cursor of
        Fresh ->
            Encode.object [ ( "tag", Encode.string "Fresh" ) ]

        Partial run ->
            Encode.object
                [ ( "tag", Encode.string "Partial" )
                , ( "run", encodePosix run.run )
                , ( "next", Encode.int run.next )
                , ( "last", maybe Encode.int run.last )
                , ( "head", maybe encodePosix run.head )
                , ( "etag", maybe Encode.string run.etag )
                ]

        Complete synced ->
            Encode.object
                [ ( "tag", Encode.string "Complete" )
                , ( "newest", maybe encodePosix synced.newest )
                , ( "etag", maybe Encode.string synced.etag )
                , ( "syncedAt", encodePosix synced.syncedAt )
                , ( "fullAt", encodePosix synced.fullAt )
                ]


posix : Decoder Time.Posix
posix =
    Decode.map Time.millisToPosix Decode.int


encodePosix : Time.Posix -> Encode.Value
encodePosix =
    Time.posixToMillis >> Encode.int


maybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
maybe encoder =
    Maybe.map encoder >> Maybe.withDefault Encode.null
