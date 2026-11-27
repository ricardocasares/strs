module Main exposing (Model, Msg, main)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Filters exposing (Choice(..), Direction(..), Facet, Filters, Key(..), Sort(..))
import Format
import GitHub
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (alt, attribute, autocomplete, class, href, id, placeholder, rel, spellcheck, src, target, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Keyed
import Html.Lazy
import Icon
import Index exposing (Index)
import InteropDefinitions as Interop exposing (Theme)
import InteropPorts
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Language
import Link
import Login exposing (Login)
import Logo
import Process
import Profile exposing (Profile)
import Repo exposing (Lifecycle(..), Origin(..), Repo)
import Set
import Sync
import Task
import Time
import Tokens
import UI.Avatar as Avatar
import UI.Badge as Badge
import UI.Button as Button
import UI.DropdownMenu as DropdownMenu
import UI.Empty as Empty
import UI.InputGroup as InputGroup
import UI.Item as Item
import UI.Kbd as Kbd
import UI.Progress as Progress
import UI.Separator as Separator
import UI.Sidebar as Sidebar
import UI.Skeleton as Skeleton
import UI.Slider as Slider
import UI.Toast as Toast
import UI.ToggleGroup as ToggleGroup
import Url exposing (Url)


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , theme : Theme
    , systemDark : Bool
    , zone : Time.Zone
    , now : Time.Posix
    , blockedUntil : Maybe Time.Posix
    , sidebar : Sidebar.State
    , toaster : Toast.Toaster Msg
    , query : String
    , filters : Filters
    , expanded : List Facet
    , sort : Sort
    , limit : Int
    , linkVersion : Int
    , page : Page
    }


{-| What the URL points at: a user's stars, or a name GitHub would never accept.
-}
type Page
    = Stars Session
    | Invalid String


type alias Session =
    { login : Login
    , account : Account
    , library : Library
    }


type Account
    = Unknown
    | Known Profile
    | Missing


type Library
    = Opening
    | Open Shelf


{-| A user's stars once they're loaded from the browser: the index, when each star was last
seen by a full sync, where syncing stands, and the results of the current search.
-}
type alias Shelf =
    { index : Index
    , seen : Dict Int Time.Posix
    , cursor : Sync.Cursor
    , sync : Sync
    , results : Results
    }


type Sync
    = Idle
    | Running Run
    | Paused Run Pause
    | Stalled Run GitHub.Problem


type alias Run =
    { job : Sync.Job
    , trigger : Trigger
    , attempt : Int
    }


{-| Who asked for the sync, which decides what is worth a toast: a first sync and anything
asked for always report back, a routine one only when it found something.
-}
type Trigger
    = Initial
    | Automatic
    | Manual


type alias Pause =
    { until : Time.Posix
    , reason : Reason
    }


type Reason
    = Limited
    | Backoff


type alias Results =
    { matches : Int
    , hits : List Repo
    , tallies : Tallies
    }


{-| How many results each value of each facet would leave, given every other filter.
-}
type alias Tallies =
    { languages : List ( String, Int )
    , topics : List ( String, Int )
    , owners : List ( String, Int )
    }


tally : Facet -> Tallies -> List ( String, Int )
tally facet tallies =
    case facet of
        Filters.Language ->
            tallies.languages

        Filters.Topic ->
            tallies.topics

        Filters.Owner ->
            tallies.owners


{-| Rows rendered at once. Two screens' worth: the rest stream in before they're scrolled to,
and every keystroke re-renders only these.
-}
pageSize : Int
pageSize =
    30


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init json url key =
    let
        flags =
            InteropPorts.decodeFlags json
                |> Result.withDefault { theme = Nothing, systemDark = False, blockedUntil = Nothing, now = 0, sidebarOpen = Nothing }

        now =
            Time.millisToPosix flags.now

        model =
            { key = key
            , theme = Maybe.withDefault Interop.System flags.theme
            , systemDark = flags.systemDark
            , zone = Time.utc
            , now = now
            , blockedUntil =
                flags.blockedUntil
                    |> Maybe.map Time.millisToPosix
                    |> Maybe.andThen
                        (\until ->
                            if Time.posixToMillis until > flags.now then
                                Just until

                            else
                                Nothing
                        )
            , sidebar =
                { open = Maybe.withDefault Sidebar.init.open flags.sidebarOpen
                , openMobile = False
                }
            , toaster = Toast.init
            , query = ""
            , filters = Filters.initial
            , expanded = []
            , sort = Relevance
            , limit = pageSize
            , linkVersion = 0
            , page = Invalid ""
            }

        ( routed, cmd ) =
            route url model
    in
    ( share (Link.parse url) routed
    , Cmd.batch
        [ cmd
        , Task.perform GotZone Time.here
        ]
    )


{-| The app lives at the document's base, `/strs/`: the base itself is the default user, and
`<base><login>` anyone else. The base isn't known here, so the URL's shape says which: the base
ends with a slash, a user's page doesn't.
-}
route : Url -> Model -> ( Model, Cmd Msg )
route url model =
    let
        segment =
            if String.endsWith "/" url.path then
                ""

            else
                String.split "/" url.path
                    |> List.reverse
                    |> List.head
                    |> Maybe.andThen Url.percentDecode
                    |> Maybe.withDefault ""

        target =
            if String.isEmpty segment then
                Just Login.default

            else
                Login.fromString segment
    in
    case ( target, model.page ) of
        ( Just login, Stars session ) ->
            if session.login == login then
                ( model, Cmd.none )

            else
                visit login model

        ( Just login, Invalid _ ) ->
            visit login model

        ( Nothing, _ ) ->
            ( { model | page = Invalid segment }, Cmd.none )


visit : Login -> Model -> ( Model, Cmd Msg )
visit login model =
    ( { model
        | page = Stars { login = login, account = Unknown, library = Opening }
        , filters = { initial | stars = model.filters.stars, lifecycle = model.filters.lifecycle, origin = model.filters.origin }
        , limit = pageSize
        , sidebar = { open = model.sidebar.open, openMobile = False }
      }
    , InteropPorts.fromElm (Interop.LoadUser (Login.toString login))
    )
        |> andThen (closeToast "sync")


initial : Filters
initial =
    Filters.initial


isDark : Model -> Bool
isDark model =
    case model.theme of
        Interop.Light ->
            False

        Interop.Dark ->
            True

        Interop.System ->
            model.systemDark



-- SEARCH


search : Model -> Index -> Results
search model index =
    let
        scored =
            case Index.score model.query index of
                Nothing ->
                    List.map (\repo -> ( repo, 0 )) (Index.toList index)

                Just scores ->
                    Dict.foldl
                        (\id relevance acc ->
                            case Index.get id index of
                                Just repo ->
                                    ( repo, relevance ) :: acc

                                Nothing ->
                                    acc
                        )
                        []
                        scores

        filters =
            model.filters

        satisfies facet repo =
            let
                wanted =
                    Filters.picked facet filters
            in
            Set.isEmpty wanted || List.any (\value -> Set.member value wanted) (Filters.valuesOf facet repo)

        bump value =
            Dict.update value (Maybe.withDefault 0 >> (+) 1 >> Just)

        count condition values dict =
            if condition then
                List.foldl bump dict values

            else
                dict

        -- Each facet is counted over the repos that every other facet lets through, so its
        -- numbers say what picking one more value would leave.
        tallyRepo (( repo, _ ) as scoredRepo) acc =
            if Filters.matches filters repo then
                let
                    inLanguage =
                        satisfies Filters.Language repo

                    inTopic =
                        satisfies Filters.Topic repo

                    inOwner =
                        satisfies Filters.Owner repo
                in
                { languages = count (inTopic && inOwner) (Filters.valuesOf Filters.Language repo) acc.languages
                , topics = count (inLanguage && inOwner) repo.topics acc.topics
                , owners = count (inLanguage && inTopic) [ repo.owner ] acc.owners
                , chosen =
                    if inLanguage && inTopic && inOwner then
                        scoredRepo :: acc.chosen

                    else
                        acc.chosen
                }

            else
                acc

        folded =
            List.foldl tallyRepo { languages = Dict.empty, topics = Dict.empty, owners = Dict.empty, chosen = [] } scored

        ranked =
            Dict.toList >> List.sortWith (\( a, x ) ( b, y ) -> compareThen (compare y x) (compare a b))
    in
    { matches = List.length folded.chosen
    , hits = List.sortWith (compareBy model.sort) folded.chosen |> List.take model.limit |> List.map Tuple.first
    , tallies =
        { languages = ranked folded.languages
        , topics = ranked folded.topics
        , owners = ranked folded.owners
        }
    }


compareBy : Sort -> ( Repo, Float ) -> ( Repo, Float ) -> Basics.Order
compareBy sort ( a, x ) ( b, y ) =
    let
        recent =
            compare (Time.posixToMillis b.starredAt) (Time.posixToMillis a.starredAt)
    in
    case sort of
        Relevance ->
            compareThen (compare y x) recent

        Sorted key direction ->
            let
                ascending =
                    case key of
                        StarredAt ->
                            compare (Time.posixToMillis a.starredAt) (Time.posixToMillis b.starredAt)

                        StarCount ->
                            compare a.stars b.stars

                        PushedAt ->
                            compare (Time.posixToMillis a.pushedAt) (Time.posixToMillis b.pushedAt)
            in
            compareThen
                (case direction of
                    Ascending ->
                        ascending

                    Descending ->
                        reverseOrder ascending
                )
                recent


reverseOrder : Basics.Order -> Basics.Order
reverseOrder order =
    case order of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT


compareThen : Basics.Order -> Basics.Order -> Basics.Order
compareThen first second =
    if first == EQ then
        second

    else
        first


{-| Runs the search again, after the query, the filters or the stars changed.
-}
refresh : Model -> Model
refresh model =
    mapShelf (\_ shelf -> { shelf | results = search model shelf.index }) model


mapShelf : (Session -> Shelf -> Shelf) -> Model -> Model
mapShelf f model =
    case model.page of
        Stars session ->
            case session.library of
                Open shelf ->
                    { model | page = Stars { session | library = Open (f session shelf) } }

                Opening ->
                    model

        Invalid _ ->
            model



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | Tick Time.Posix
    | GotZone Time.Zone
    | GotInterop (Result Decode.Error Interop.ToElm)
    | SidebarChanged Sidebar.State
    | ToastMsg (Toast.Msg Msg)
    | ThemeSelected Theme
    | QueryChanged String
    | QueryCleared
    | SearchFocused
    | Ignored
    | LinkSettled Int
    | LifecycleChosen (Choice Lifecycle)
    | OriginChosen (Choice Origin)
    | StarsChanged ( Float, Float )
    | Picked Facet String
    | Cleared Facet
    | MoreToggled Facet
    | FiltersReset
    | SortChosen Sort
    | MoreRequested
    | SyncRequested Sync.Mode
    | RetryRequested
    | GotPage Login Sync.Job ( GitHub.Response (List Repo), Time.Posix )
    | GotProfile Login ( GitHub.Response (Time.Posix -> Profile), Time.Posix )
    | Resumed Login Time.Posix
    | VisibilityChanged Browser.Events.Visibility
    | ProfileOpened String


{-| Every change to what a link carries rewrites the URL, once typing has paused: browsers
limit how often a page may replace its URL, and the history needs no entry per keystroke.
-}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( next, cmd ) =
            handle msg model
    in
    if shared next /= shared model then
        ( { next | linkVersion = next.linkVersion + 1 }
        , Cmd.batch [ cmd, Task.perform (\_ -> LinkSettled (next.linkVersion + 1)) (Process.sleep 400) ]
        )

    else
        ( next, cmd )


shared : Model -> Link.Shared
shared model =
    { query = model.query, filters = model.filters, sort = model.sort }


{-| Shows what a link carries.
-}
share : Link.Shared -> Model -> Model
share link model =
    refresh { model | query = link.query, filters = link.filters, sort = link.sort, limit = pageSize }


handle : Msg -> Model -> ( Model, Cmd Msg )
handle msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External url) ->
            ( model, Nav.load url )

        -- A link to another user can carry filters; the app's own URL rewrites never change
        -- the user, so their query is what the model already shows.
        UrlChanged url ->
            let
                ( next, cmd ) =
                    route url model
            in
            if loginOf next /= loginOf model && url.query /= Nothing then
                ( share (Link.parse url) next, cmd )

            else
                ( next, cmd )

        LinkSettled version ->
            if version == model.linkVersion then
                case model.page of
                    Stars session ->
                        ( model, Nav.replaceUrl model.key (Login.toString session.login ++ Link.toQuery (shared model)) )

                    Invalid _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        Tick now ->
            ( { model | now = now }, Cmd.none )

        GotZone zone ->
            ( { model | zone = zone }, Cmd.none )

        GotInterop (Ok message) ->
            fromJs message model

        GotInterop (Err _) ->
            ( model, Cmd.none )

        -- The panel's state outlives the visit; the mobile sheet always starts closed.
        SidebarChanged sidebar ->
            ( { model | sidebar = sidebar }
            , if sidebar.open /= model.sidebar.open then
                InteropPorts.fromElm
                    (Interop.SaveSetting
                        { key = "sidebar"
                        , value =
                            Just
                                (if sidebar.open then
                                    "open"

                                 else
                                    "collapsed"
                                )
                        }
                    )

              else
                Cmd.none
            )

        ToastMsg toastMsg ->
            Toast.update ToastMsg toastMsg model.toaster
                |> Tuple.mapFirst (\toaster -> { model | toaster = toaster })

        ThemeSelected theme ->
            ( { model | theme = theme }
            , InteropPorts.fromElm (Interop.SaveSetting { key = "theme", value = Just (Interop.themeToString theme) })
            )

        QueryChanged query ->
            ( refresh { model | query = query, limit = pageSize }, Cmd.none )

        QueryCleared ->
            ( refresh { model | query = "", limit = pageSize }, focus "search" )

        SearchFocused ->
            ( model, focus "search" )

        Ignored ->
            ( model, Cmd.none )

        LifecycleChosen choice ->
            setFilters (\filters -> { filters | lifecycle = choice }) model

        OriginChosen choice ->
            setFilters (\filters -> { filters | origin = choice }) model

        StarsChanged range ->
            setFilters (\filters -> { filters | stars = Filters.starsFromSlider range }) model

        Picked facet value ->
            setFilters (Filters.pick facet value) model

        Cleared facet ->
            setFilters (Filters.clear facet) model

        MoreToggled facet ->
            ( { model
                | expanded =
                    if List.member facet model.expanded then
                        List.filter ((/=) facet) model.expanded

                    else
                        facet :: model.expanded
              }
            , Cmd.none
            )


        FiltersReset ->
            setFilters (\_ -> Filters.initial) model

        SortChosen sort ->
            ( refresh { model | sort = sort, limit = pageSize }, Cmd.none )

        MoreRequested ->
            ( refresh { model | limit = model.limit + pageSize }, Cmd.none )

        SyncRequested mode ->
            withSession (startSync Manual mode) model

        RetryRequested ->
            withSession retry { model | blockedUntil = Nothing }

        GotPage login job ( response, now ) ->
            withSessionOf login (gotPage job response) (observe response { model | now = now })

        GotProfile login ( response, now ) ->
            withSessionOf login (gotProfile response) (observe response { model | now = now })

        Resumed login until ->
            withSessionOf login (resume until) model

        VisibilityChanged Browser.Events.Visible ->
            withSession (startIfDue (10 * 60 * 1000)) model

        VisibilityChanged Browser.Events.Hidden ->
            ( model, Cmd.none )

        ProfileOpened url ->
            ( model, InteropPorts.fromElm (Interop.OpenUrl url) )


loginOf : Model -> Maybe Login
loginOf model =
    case model.page of
        Stars session ->
            Just session.login

        Invalid _ ->
            Nothing


setFilters : (Filters -> Filters) -> Model -> ( Model, Cmd Msg )
setFilters change model =
    ( refresh { model | filters = change model.filters, limit = pageSize }, Cmd.none )


focus : String -> Cmd Msg
focus elementId =
    Task.attempt (\_ -> Ignored) (Browser.Dom.focus elementId)


fromJs : Interop.ToElm -> Model -> ( Model, Cmd Msg )
fromJs message model =
    case message of
        Interop.SystemThemeChanged dark ->
            ( { model | systemDark = dark }, Cmd.none )

        Interop.UserLoaded loaded ->
            case model.page of
                Stars session ->
                    if Login.toString session.login == loaded.login && session.library == Opening then
                        opened session loaded model

                    else
                        ( model, Cmd.none )

                Invalid _ ->
                    ( model, Cmd.none )

        Interop.StorageFailed _ ->
            toast
                (Toast.new
                    |> Toast.id "storage"
                    |> Toast.title "Couldn't save to this browser"
                    |> Toast.error
                )
                model


{-| The user's stars are out of the database: index them, search them, and sync if it's due.
-}
opened : Session -> { login : String, repos : Encode.Value, account : Encode.Value } -> Model -> ( Model, Cmd Msg )
opened session loaded model =
    let
        stored =
            Decode.decodeValue (Decode.list storedRepo) loaded.repos
                |> Result.withDefault []

        account =
            Decode.decodeValue (Decode.nullable accountDecoder) loaded.account
                |> Result.toMaybe
                |> Maybe.andThen identity

        index =
            Index.fromList (List.map Tuple.first stored)

        shelf =
            { index = index
            , seen = Dict.fromList (List.map (\( repo, seen ) -> ( repo.id, seen )) stored)
            , cursor = Maybe.map .cursor account |> Maybe.withDefault Sync.Fresh
            , sync = Idle
            , results = search model index
            }

        next =
            { session
                | library = Open shelf
                , account =
                    case Maybe.andThen .profile account of
                        Just profile ->
                            Known profile

                        Nothing ->
                            Unknown
            }
    in
    startIfDue (5 * 60 * 1000) next { model | page = Stars next }


storedRepo : Decoder ( Repo, Time.Posix )
storedRepo =
    Decode.map2 Tuple.pair
        Repo.storedDecoder
        (Decode.field "seen" (Decode.map Time.millisToPosix Decode.int))


{-| What the browser keeps about a user besides their stars: their profile, and where syncing
them stands.
-}
type alias Stored =
    { profile : Maybe Profile
    , cursor : Sync.Cursor
    }


accountDecoder : Decoder Stored
accountDecoder =
    Decode.map2 Stored
        (Decode.field "profile" (Decode.nullable Profile.storedDecoder))
        (Decode.field "cursor" Sync.cursorDecoder)


toast : Toast.Toast Toast.Ready Msg -> Model -> ( Model, Cmd Msg )
toast item model =
    update (ToastMsg (Toast.add item)) model


closeToast : String -> Model -> ( Model, Cmd Msg )
closeToast toastId model =
    update (ToastMsg (Toast.close toastId)) model


andThen : (Model -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen f ( model, cmd ) =
    let
        ( next, nextCmd ) =
            f model
    in
    ( next, Cmd.batch [ cmd, nextCmd ] )



-- SYNC


withSession : (Session -> Model -> ( Model, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
withSession f model =
    case model.page of
        Stars session ->
            f session model

        Invalid _ ->
            ( model, Cmd.none )


{-| Responses for a user that is no longer on screen are dropped.
-}
withSessionOf : Login -> (Session -> Model -> ( Model, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
withSessionOf login f model =
    withSession
        (\session m ->
            if session.login == login then
                f session m

            else
                ( m, Cmd.none )
        )
        model


setSession : Session -> Model -> Model
setSession session model =
    { model | page = Stars session }


setSync : Session -> Shelf -> Sync -> Model -> Model
setSync session shelf sync =
    setSession { session | library = Open { shelf | sync = sync } }


startIfDue : Int -> Session -> Model -> ( Model, Cmd Msg )
startIfDue age session model =
    case session.library of
        Open shelf ->
            let
                due =
                    case Sync.syncedAt shelf.cursor of
                        Just at ->
                            Time.posixToMillis model.now - Time.posixToMillis at > age

                        Nothing ->
                            True
            in
            if due then
                startSync Automatic Sync.Refresh session model

            else
                ( model, Cmd.none )

        Opening ->
            ( model, Cmd.none )


startSync : Trigger -> Sync.Mode -> Session -> Model -> ( Model, Cmd Msg )
startSync trigger mode session model =
    case ( session.library, session.account ) of
        ( _, Missing ) ->
            ( model, Cmd.none )

        ( Open shelf, _ ) ->
            case shelf.sync of
                Running _ ->
                    ( model, Cmd.none )

                Paused _ _ ->
                    retry session { model | blockedUntil = Nothing }

                _ ->
                    let
                        actual =
                            case ( trigger, shelf.cursor ) of
                                ( Automatic, Sync.Complete _ ) ->
                                    Automatic

                                ( Automatic, _ ) ->
                                    Initial

                                _ ->
                                    trigger
                    in
                    fetch session shelf { job = Sync.plan mode model.now shelf.cursor, trigger = actual, attempt = 0 } model
                        |> andThen (refreshProfile session)

        ( Opening, _ ) ->
            ( model, Cmd.none )


refreshProfile : Session -> Model -> ( Model, Cmd Msg )
refreshProfile session model =
    let
        request etag =
            ( model
            , GitHub.profile session.login etag
                |> Task.andThen (\response -> Task.map (Tuple.pair response) Time.now)
                |> Task.perform (GotProfile session.login)
            )
    in
    case session.account of
        Unknown ->
            request Nothing

        Known profile ->
            if Profile.isStale model.now profile then
                request profile.etag

            else
                ( model, Cmd.none )

        Missing ->
            ( model, Cmd.none )


{-| Asks for the next page, unless GitHub already said to wait.
-}
fetch : Session -> Shelf -> Run -> Model -> ( Model, Cmd Msg )
fetch session shelf run model =
    case model.blockedUntil of
        Just until ->
            if Time.posixToMillis until > Time.posixToMillis model.now then
                pause session shelf run { until = until, reason = Limited } model

            else
                fetch session shelf run { model | blockedUntil = Nothing }

        Nothing ->
            ( setSync session shelf (Running run) model
            , GitHub.starred session.login (Sync.request run.job)
                |> Task.andThen (\response -> Task.map (Tuple.pair response) Time.now)
                |> Task.perform (GotPage session.login run.job)
            )


pause : Session -> Shelf -> Run -> Pause -> Model -> ( Model, Cmd Msg )
pause session shelf run waiting model =
    let
        wait =
            max 0 (Time.posixToMillis waiting.until - Time.posixToMillis model.now)

        resumeCmd =
            Task.perform (\_ -> Resumed session.login waiting.until) (Process.sleep (toFloat wait))

        paused =
            setSync session shelf (Paused run waiting) model
    in
    case waiting.reason of
        Limited ->
            ( paused, resumeCmd )
                |> andThen
                    (toast
                        (Toast.new
                            |> Toast.id "sync"
                            |> Toast.title ("Rate limited until " ++ Format.clock model.zone waiting.until)
                            |> Toast.warning
                            |> Toast.timeout 0
                            |> Toast.action "Retry" RetryRequested
                        )
                    )

        Backoff ->
            ( paused, resumeCmd )


resume : Time.Posix -> Session -> Model -> ( Model, Cmd Msg )
resume until session model =
    case session.library of
        Open shelf ->
            case shelf.sync of
                Paused run waiting ->
                    if waiting.until == until then
                        fetch session shelf run { model | now = until }
                            |> andThen
                                (if waiting.reason == Limited then
                                    toast
                                        (Toast.new
                                            |> Toast.id "sync"
                                            |> Toast.title "Syncing stars…"
                                            |> Toast.loading
                                        )

                                 else
                                    \m -> ( m, Cmd.none )
                                )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Opening ->
            ( model, Cmd.none )


retry : Session -> Model -> ( Model, Cmd Msg )
retry session model =
    case session.library of
        Open shelf ->
            case shelf.sync of
                Paused run _ ->
                    fetch session shelf { run | trigger = Manual } model |> andThen (closeToast "sync")

                Stalled run _ ->
                    fetch session shelf { run | trigger = Manual, attempt = 0 } model |> andThen (closeToast "sync")

                Idle ->
                    startSync Manual Sync.Refresh session model

                Running _ ->
                    ( model, Cmd.none )

        Opening ->
            ( model, Cmd.none )


{-| Every response says how many requests are left. When none are, nothing is sent until the
window resets, and the reset is remembered across reloads.
-}
observe : GitHub.Response a -> Model -> Model
observe response model =
    let
        quota =
            case response of
                GitHub.Fresh _ meta ->
                    meta.quota

                GitHub.NotModified meta ->
                    meta.quota

                _ ->
                    Nothing
    in
    case quota of
        Just { remaining, resetsAt } ->
            if remaining == 0 then
                { model | blockedUntil = Just resetsAt }

            else
                model

        Nothing ->
            model


rememberBlock : Model -> Cmd Msg
rememberBlock model =
    InteropPorts.fromElm
        (Interop.SaveSetting
            { key = "blockedUntil"
            , value = Maybe.map (Time.posixToMillis >> String.fromInt) model.blockedUntil
            }
        )


gotPage : Sync.Job -> GitHub.Response (List Repo) -> Session -> Model -> ( Model, Cmd Msg )
gotPage job response session model =
    case session.library of
        Open shelf ->
            case shelf.sync of
                Running run ->
                    if run.job /= job then
                        ( model, Cmd.none )

                    else
                        handlePage run response session shelf model
                            |> andThen (\m -> ( m, rememberBlock m ))

                _ ->
                    ( model, Cmd.none )

        Opening ->
            ( model, Cmd.none )


handlePage : Run -> GitHub.Response (List Repo) -> Session -> Shelf -> Model -> ( Model, Cmd Msg )
handlePage run response session shelf model =
    case response of
        GitHub.Fresh repos meta ->
            step run (Sync.advance model.now { repos = repos, lastPage = meta.lastPage, etag = meta.etag } run.job) session shelf model

        GitHub.NotModified _ ->
            case Sync.unchanged model.now run.job of
                Just next ->
                    step run next session shelf model

                Nothing ->
                    failed run GitHub.Unreadable session shelf model

        GitHub.NotFound ->
            ( setSession { session | account = Missing, library = Open { shelf | sync = Idle } } model, Cmd.none )
                |> andThen (closeToast "sync")

        GitHub.RateLimited wait ->
            let
                until =
                    case wait of
                        GitHub.RetryAfter seconds ->
                            Time.millisToPosix (Time.posixToMillis model.now + seconds * 1000)

                        GitHub.Until at ->
                            at

                        GitHub.Unspecified ->
                            Time.millisToPosix (Time.posixToMillis model.now + 60 * 1000)
            in
            pause session shelf run { until = until, reason = Limited } { model | blockedUntil = Just until }

        GitHub.Failed problem ->
            failed run problem session shelf model


{-| Network trouble is retried quietly, a few times, backing off; then the user decides.
-}
failed : Run -> GitHub.Problem -> Session -> Shelf -> Model -> ( Model, Cmd Msg )
failed run problem session shelf model =
    if run.attempt < 3 then
        pause session
            shelf
            { run | attempt = run.attempt + 1 }
            { until = Time.millisToPosix (Time.posixToMillis model.now + 2000 * 2 ^ run.attempt), reason = Backoff }
            model

    else
        ( setSync session shelf (Stalled run problem) model, Cmd.none )
            |> andThen
                (toast
                    (Toast.new
                        |> Toast.id "sync"
                        |> Toast.title
                            (case problem of
                                GitHub.Offline ->
                                    "You're offline"

                                _ ->
                                    "Couldn't sync stars"
                            )
                        |> Toast.error
                        |> Toast.timeout 0
                        |> Toast.action "Retry" RetryRequested
                    )
                )


step : Run -> Sync.Step -> Session -> Shelf -> Model -> ( Model, Cmd Msg )
step run next session shelf model =
    case next of
        Sync.Continue job save ->
            let
                ( saved, cmd ) =
                    store save session shelf model
            in
            fetch session saved { run | job = job, attempt = 0 } model
                |> Tuple.mapSecond (\fetchCmd -> Cmd.batch [ cmd, fetchCmd ])
                |> Tuple.mapFirst refresh

        Sync.Done finish save ->
            let
                ( saved, cmd ) =
                    store save session shelf model

                ( pruned, pruneCmd, removed ) =
                    case finish of
                        Sync.Rebuilt { prune } ->
                            prune_ prune session saved

                        Sync.Caught _ ->
                            ( saved, Cmd.none, 0 )

                report =
                    case finish of
                        Sync.Caught 0 ->
                            if run.trigger == Manual then
                                Just (Toast.new |> Toast.title "Up to date" |> Toast.success)

                            else
                                Nothing

                        Sync.Caught added ->
                            Just
                                (Toast.new
                                    |> Toast.title
                                        (if added == 1 then
                                            "1 new star"

                                         else
                                            Format.thousands added ++ " new stars"
                                        )
                                    |> Toast.success
                                )

                        Sync.Rebuilt _ ->
                            if run.trigger /= Automatic || removed > 0 then
                                Just
                                    (Toast.new
                                        |> Toast.title (Format.thousands (Index.size pruned.index) ++ " stars synced")
                                        |> Toast.success
                                    )

                            else
                                Nothing
            in
            ( refresh (setSync session pruned Idle model), Cmd.batch [ cmd, pruneCmd ] )
                |> andThen
                    (case report of
                        Just item ->
                            toast (Toast.id "sync" item)

                        Nothing ->
                            closeToast "sync"
                    )


{-| Saves a page in the index and the database, with the cursor that follows it.
-}
store : Sync.Save -> Session -> Shelf -> Model -> ( Shelf, Cmd Msg )
store save session shelf model =
    let
        login =
            Login.toString session.login

        saved =
            { shelf
                | index = Index.upsert save.repos shelf.index
                , seen = List.foldl (\repo -> Dict.insert repo.id save.seen) shelf.seen save.repos
                , cursor = save.cursor
            }
    in
    ( saved
    , InteropPorts.fromElm
        (Interop.Write
            (List.map (\repo -> Interop.Put Interop.Repos (Repo.encode { login = login, seen = save.seen } repo)) save.repos
                ++ [ Interop.Put Interop.Accounts (encodeAccount session.account saved model) ]
            )
        )
    )


{-| After a full sync, the stars it didn't see again were removed from GitHub. The index is
rebuilt from scratch too, so its field averages are exact again.
-}
prune_ : Time.Posix -> Session -> Shelf -> ( Shelf, Cmd Msg, Int )
prune_ run session shelf =
    let
        stale =
            Dict.foldl
                (\id seen acc ->
                    if seen /= run then
                        id :: acc

                    else
                        acc
                )
                []
                shelf.seen

        kept =
            { shelf
                | index = Index.fromList (Index.toList (Index.remove stale shelf.index))
                , seen = List.foldl Dict.remove shelf.seen stale
            }
    in
    ( kept
    , if List.isEmpty stale then
        Cmd.none

      else
        InteropPorts.fromElm
            (Interop.Write
                (List.map
                    (\id -> Interop.Delete Interop.Repos (Encode.list identity [ Encode.string (Login.toString session.login), Encode.int id ]))
                    stale
                )
            )
    , List.length stale
    )


gotProfile : GitHub.Response (Time.Posix -> Profile) -> Session -> Model -> ( Model, Cmd Msg )
gotProfile response session model =
    let
        save account =
            let
                next =
                    { session | account = account }
            in
            case session.library of
                Open shelf ->
                    ( setSession next model
                    , InteropPorts.fromElm (Interop.Write [ Interop.Put Interop.Accounts (encodeAccount account shelf model) ])
                    )

                Opening ->
                    ( setSession next model, Cmd.none )
    in
    case ( response, session.account ) of
        ( GitHub.Fresh toProfile _, _ ) ->
            save (Known (toProfile model.now))

        ( GitHub.NotModified _, Known profile ) ->
            save (Known { profile | fetchedAt = model.now })

        ( GitHub.NotFound, _ ) ->
            ( setSession { session | account = Missing } model, Cmd.none )

        _ ->
            ( model, Cmd.none )


encodeAccount : Account -> Shelf -> Model -> Encode.Value
encodeAccount account shelf model =
    case model.page of
        Stars session ->
            Encode.object
                [ ( "login", Encode.string (Login.toString session.login) )
                , ( "profile"
                  , case account of
                        Known profile ->
                            Profile.encode profile

                        _ ->
                            Encode.null
                  )
                , ( "cursor", Sync.encodeCursor shelf.cursor )
                ]

        Invalid _ ->
            Encode.null



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map GotInterop InteropPorts.toElm
        , Toast.subscriptions ToastMsg model.toaster
        , Time.every 30000 Tick
        , Browser.Events.onVisibilityChange VisibilityChanged
        , Browser.Events.onKeyDown shortcut
        ]


{-| `/` or ⌘K focuses the search, unless a field already has the keyboard. Every other key
fails the decoder, so typing doesn't send the app a message per keystroke.
-}
shortcut : Decoder Msg
shortcut =
    Decode.map4
        (\key meta ctrl tag ->
            not (List.member tag [ "INPUT", "TEXTAREA", "SELECT" ])
                && (key == "/" || ((meta || ctrl) && key == "k"))
        )
        (Decode.field "key" Decode.string)
        (Decode.field "metaKey" Decode.bool)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.oneOf [ Decode.at [ "target", "tagName" ] Decode.string, Decode.succeed "" ])
        |> Decode.andThen
            (\isShortcut ->
                if isShortcut then
                    Decode.succeed SearchFocused

                else
                    Decode.fail "not a shortcut"
            )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title =
        case model.page of
            Stars session ->
                Login.toString session.login ++ " · stars"

            Invalid _ ->
                "stars"
    , body =
        [ div
            [ class
                (if isDark model then
                    "dark"

                 else
                    ""
                )
            ]
            [ Sidebar.new
                |> Sidebar.state model.sidebar SidebarChanged
                |> Sidebar.header [ viewLogo model ]
                |> Sidebar.content
                    (let
                        filters =
                            viewFilters model
                     in
                     viewFacets model ++ [ viewOrder model, filters.stars, filters.lifecycle, filters.origin ]
                    )
                |> Sidebar.footer [ viewAccount model ]
                |> Sidebar.rail
                |> Sidebar.page [ class "min-w-0" ]
                    [ viewTopBar model
                    , Html.section [ class "mx-auto w-full max-w-3xl px-2 pt-2 pb-16 md:px-4" ] [ viewMain model ]
                    ]
                |> Sidebar.toHtml []
            , Toast.view ToastMsg model.toaster
            ]
        ]
    }



-- ACCOUNT


{-| The app's mark: home is the default user's stars.
-}
viewLogo : Model -> Html Msg
viewLogo model =
    Sidebar.menu []
        [ Sidebar.menuItem []
            [ Html.a
                [ href "./"
                , attribute "aria-label" "strs"
                , class "flex h-12 items-center rounded-md px-2 text-sidebar-foreground ring-sidebar-ring outline-hidden focus-visible:ring-2"
                ]
                [ Logo.view "text-2xl [-webkit-text-stroke:0.75px_currentColor]" ]
            ]
        ]


viewAccount : Model -> Html Msg
viewAccount model =
    case model.page of
        Stars session ->
            DropdownMenu.new
                |> DropdownMenu.id "account"
                |> DropdownMenu.trigger (Sidebar.menuButtonAttributes ++ [ Sidebar.large, class "data-[size=lg]:h-12" ])
                    [ viewIdentity session
                    , Icon.view "ms-auto size-4 text-muted-foreground" Icon.chevronsUpDown
                    ]
                |> DropdownMenu.content (accountMenu model session)
                |> DropdownMenu.right
                |> DropdownMenu.alignEnd
                |> DropdownMenu.toHtml [ class "w-56 max-w-[calc(100vw-2rem)] rounded-lg" ]

        Invalid _ ->
            text ""


viewIdentity : Session -> Html Msg
viewIdentity session =
    let
        login =
            Login.toString session.login

        total =
            case session.library of
                Open shelf ->
                    if Index.size shelf.index > 0 then
                        Just (Index.size shelf.index)

                    else
                        Nothing

                Opening ->
                    Nothing

        subtitle =
            span [ class "truncate text-xs text-muted-foreground" ]
                [ text ("@" ++ login)
                , case total of
                    Just count ->
                        text (" · " ++ Format.thousands count ++ " ★")

                    Nothing ->
                        text ""
                ]

        layout avatar title =
            span [ class "flex min-w-0 flex-1 items-center gap-2" ]
                [ avatar
                , span [ class "grid min-w-0 flex-1 text-start leading-tight" ] [ title, subtitle ]
                ]
    in
    case session.account of
        Known profile ->
            layout
                (tile "size-8 rounded-lg" login (Just profile.avatar))
                (span [ class "truncate text-sm font-medium" ] [ text (Profile.displayName profile) ])

        Unknown ->
            if isWaiting session then
                layout
                    (tile "size-8 rounded-lg" login Nothing)
                    (span [ class "truncate text-sm font-medium" ] [ text login ])

            else
                layout
                    (Skeleton.view [ class "size-8 shrink-0 rounded-lg!" ])
                    (span [ class "flex h-5 items-center" ] [ Skeleton.view [ class "h-3.5 w-24" ] ])

        Missing ->
            layout
                (span [ class "flex size-8 shrink-0 items-center justify-center rounded-lg bg-muted text-muted-foreground" ] [ Icon.view "" Icon.userX ])
                (span [ class "truncate text-sm font-medium" ] [ text login ])


{-| A user's avatar in a rounded square, over their initial. The image gets an opaque
background, so logos with transparent parts don't show the initial through.
-}
tile : String -> String -> Maybe String -> Html msg
tile classes login avatar =
    span
        [ class ("relative flex shrink-0 items-center justify-center overflow-hidden bg-muted text-xs font-medium text-muted-foreground " ++ classes)
        , attribute "aria-hidden" "true"
        ]
        [ text (String.toUpper (String.left 1 login))
        , case avatar of
            Just url ->
                Html.img [ src (Profile.avatar 80 url), alt "", class "absolute inset-0 size-full bg-background object-cover" ] []

            Nothing ->
                text ""
        ]


{-| Two sections: these stars, and the theme.
-}
accountMenu : Model -> Session -> List (DropdownMenu.Item Msg)
accountMenu model session =
    let
        current =
            Login.toString session.login

        action attrs icon label =
            DropdownMenu.item attrs [ Icon.view "" icon, text label ]

        sync =
            case ( session.account, session.library ) of
                ( Missing, _ ) ->
                    []

                ( _, Open shelf ) ->
                    [ syncItem model session shelf
                    , action [ onClick (SyncRequested Sync.Rebuild) ] Icon.refresh "Resync all"
                        |> (if isSyncing session then
                                DropdownMenu.disabled

                            else
                                identity
                           )
                    ]

                ( _, Opening ) ->
                    []
    in
    [ DropdownMenu.label [ text "Stars" ]
    , DropdownMenu.group
        (action [ onClick (ProfileOpened ("https://github.com/" ++ current ++ "?tab=stars")) ] Icon.external "Open on GitHub"
            :: sync
        )
    , DropdownMenu.separator
    , DropdownMenu.label [ text "Theme" ]
    , DropdownMenu.label [ div [ class "pb-1" ] [ themeSwitch model ] ]
    ]


{-| Syncing is held up for longer than a skeleton should pulse.
-}
isWaiting : Session -> Bool
isWaiting session =
    case session.library of
        Open shelf ->
            case shelf.sync of
                Paused _ { reason } ->
                    reason == Limited

                Stalled _ _ ->
                    True

                _ ->
                    False

        Opening ->
            False


isSyncing : Session -> Bool
isSyncing session =
    case session.library of
        Open shelf ->
            case shelf.sync of
                Running _ ->
                    True

                _ ->
                    False

        Opening ->
            False



-- FILTERS


{-| One section, because everything in it narrows the same list: "Clear all" means all of it.
-}
viewFilters : Model -> { lifecycle : Html Msg, origin : Html Msg, stars : Html Msg }
viewFilters model =
    let
        filters =
            model.filters

        group icon label aside content =
            Sidebar.group []
                [ Sidebar.groupLabel [ class "justify-between pe-1" ] (sectionTitle icon label :: aside)
                , Sidebar.groupContent [ class "px-1" ] content
                ]
    in
    { lifecycle =
        group Icon.archive "Lifecycle"
        []
        [ segmented "Lifecycle"
            LifecycleChosen
            filters.lifecycle
            [ ( Any, "All" ), ( Only Active, "Active" ), ( Only Archived, "Archived" ) ]
        ]
    , origin =
        group Icon.fork "Origin"
        []
        [ segmented "Origin"
            OriginChosen
            filters.origin
            [ ( Any, "All" ), ( Only Source, "Sources" ), ( Only Fork, "Forks" ) ]
        ]
    , stars =
        group Icon.star "Stars"
        [ if Filters.isAnyStars filters.stars then
            text ""

          else
            -- The range, as a badge that resets it.
            clearBadge (Filters.starsLabel filters.stars)
                ("Clear stars: " ++ Filters.starsLabel filters.stars)
                (StarsChanged ( 0, toFloat Filters.tierCount ))
        ]
        [ div [ class "px-1 pt-1" ]
            [ Slider.new
                |> Slider.range (Tuple.first (Filters.starsToSlider filters.stars)) (Tuple.second (Filters.starsToSlider filters.stars))
                |> Slider.onChange StarsChanged
                |> Slider.min 0
                |> Slider.max (toFloat Filters.tierCount)
                |> Slider.step 1
                |> Slider.toHtml [ attribute "aria-label" "Stars" ]
            , div [ class "mt-2 flex justify-between text-[10px] text-muted-foreground tabular-nums" ]
                (List.map (\label -> span [] [ text label ]) [ "0", "10", "100", "1k", "10k", "100k", "∞" ])
            ]
        ]
    }


{-| A group label's icon and text.
-}
sectionTitle : Icon.Icon -> String -> Html msg
sectionTitle icon label =
    span [ class "flex items-center gap-2" ] [ Icon.view "" icon, text label ]


facetIcon : Facet -> Icon.Icon
facetIcon facet =
    case facet of
        Filters.Language ->
            Icon.code

        Filters.Topic ->
            Icon.hash

        Filters.Owner ->
            Icon.users


{-| A small primary badge beside a group label: what's set there, and a cross that clears it.
-}
clearBadge : String -> String -> Msg -> Html Msg
clearBadge content label msg =
    Html.button
        ([ type_ "button", onClick msg, attribute "aria-label" label ]
            ++ Badge.toAttributes (Badge.new |> Badge.text "")
            ++ [ class "h-4! cursor-pointer gap-0.5! px-1.5! text-[10px]! leading-none tabular-nums hover:bg-primary/85 [&_svg[class]]:size-2.5!" ]
        )
        [ text content, Icon.view "" Icon.close ]


{-| Exactly one option is always pressed: picking another moves the press.
-}
segmented : String -> (Choice a -> Msg) -> Choice a -> List ( Choice a, String ) -> Html Msg
segmented label toMsg current options =
    ToggleGroup.new
        |> ToggleGroup.onToggle toMsg
        |> ToggleGroup.pressed [ current ]
        |> ToggleGroup.outline
        |> ToggleGroup.small
        |> ToggleGroup.spacing 0
        |> ToggleGroup.items (List.map (\( value, name ) -> ToggleGroup.item value [ text name ]) options)
        |> ToggleGroup.toHtml
            [ class "w-full *:h-6 *:flex-1 *:text-xs"

            -- The pressed option is filled like the badges, which reads in both themes.
            , class "*:aria-pressed:border-primary *:aria-pressed:bg-primary *:aria-pressed:text-primary-foreground *:aria-pressed:hover:bg-primary/90 *:aria-pressed:hover:text-primary-foreground"
            , attribute "aria-label" label
            ]


{-| Languages, topics and users, each a section that starts collapsed. A section's header
counts its picks, so filters hidden inside a collapsed section are still in view.
-}
viewFacets : Model -> List (Html Msg)
viewFacets model =
    let
        tallies =
            case model.page of
                Stars session ->
                    case ( session.account, session.library ) of
                        ( Missing, _ ) ->
                            Nothing

                        ( _, Opening ) ->
                            Just Nothing

                        ( _, Open shelf ) ->
                            if isFilling shelf then
                                Just Nothing

                            else
                                Just (Just shelf.results.tallies)

                Invalid _ ->
                    Nothing
    in
    case tallies of
        Just counted ->
            List.map (viewFacet model counted) Filters.facets

        Nothing ->
            []


{-| A facet's values, under its label. While some are picked, a badge beside the label
counts them and clears them.
-}
viewFacet : Model -> Maybe Tallies -> Facet -> Html Msg
viewFacet model tallies facet =
    let
        wanted =
            Filters.picked facet model.filters
    in
    Sidebar.group []
        [ Sidebar.groupLabel [ class "justify-between pe-1" ]
            [ sectionTitle (facetIcon facet) (Filters.facetLabel facet)
            , if Set.isEmpty wanted then
                text ""

              else
                clearBadge (String.fromInt (Set.size wanted))
                    ("Clear " ++ String.toLower (Filters.facetLabel facet))
                    (Cleared facet)
            ]
        , Sidebar.menu []
            (case tallies of
                Just counted ->
                    facetEntries model facet wanted counted

                Nothing ->
                    -- Five values with their counts, and the "more" row: the list as it lands.
                    List.map
                        (\width ->
                            Sidebar.menuItem []
                                [ Sidebar.menuSkeleton [ Sidebar.showIcon, class width ]
                                , Sidebar.menuBadge [ class "top-1.5" ] [ Skeleton.view [ class "h-3 w-5" ] ]
                                ]
                        )
                        [ "[--skeleton-width:55%]", "[--skeleton-width:70%]", "[--skeleton-width:45%]", "[--skeleton-width:60%]", "[--skeleton-width:50%]" ]
                        ++ [ Sidebar.menuItem [] [ Sidebar.menuSkeleton [ Sidebar.showIcon, class "[--skeleton-width:30%]" ] ] ]
            )
        ]


facetEntries : Model -> Facet -> Set.Set String -> Tallies -> List (Html Msg)
facetEntries model facet wanted tallies =
    let
        counted =
            tally facet tallies

        -- Picked values stay listed even when nothing else would match them, so they can be
        -- unpicked.
        listed =
            counted
                ++ (Set.toList wanted
                        |> List.filter (\value -> not (List.any (Tuple.first >> (==) value) counted))
                        |> List.map (\value -> ( value, 0 ))
                   )

        expanded =
            List.member facet model.expanded

        shown =
            listed
                |> List.indexedMap Tuple.pair
                |> List.filter
                    (\( index, ( value, _ ) ) ->
                        index
                            < (if expanded then
                                100

                               else
                                5
                              )
                            || Set.member value wanted
                    )
                |> List.map Tuple.second

        hidden =
            min 100 (List.length listed) - List.length shown

        toggle icon label =
            Sidebar.menuItem []
                [ Sidebar.menuButton [ onClick (MoreToggled facet), class "text-sidebar-foreground/70" ]
                    [ Icon.view "" icon, span [] [ text label ] ]
                ]
    in
    List.map (facetItem facet wanted (\owner -> shelfOf model |> Maybe.andThen (.index >> Index.avatar owner))) shown
        ++ (if hidden > 0 then
                [ toggle Icon.caretDown (String.fromInt hidden ++ " more") ]

            else if expanded && List.length listed > 5 then
                [ toggle Icon.caretUp "Show less" ]

            else
                []
           )
        ++ (if List.isEmpty listed then
                [ Sidebar.menuItem [] [ div [ class "flex h-8 items-center px-2 text-sm text-muted-foreground" ] [ text "None" ] ] ]

            else
                []
           )


facetItem : Facet -> Set.Set String -> (String -> Maybe String) -> ( String, Int ) -> Html Msg
facetItem facet wanted avatarOf ( value, count ) =
    let
        isPicked =
            Set.member value wanted
    in
    Sidebar.menuItem []
        [ Sidebar.menuButton
            ([ onClick (Picked facet value), attribute "aria-pressed" (boolString isPicked) ]
                ++ (if isPicked then
                        [ Sidebar.active ]

                    else
                        []
                   )
            )
            [ case facet of
                Filters.Language ->
                    span [ class "flex size-4 shrink-0 items-center justify-center" ] [ languageDot value "size-2.5" ]

                Filters.Topic ->
                    span [ class "flex size-4 shrink-0 items-center justify-center text-xs text-muted-foreground" ] [ text "#" ]

                Filters.Owner ->
                    case avatarOf value of
                        Just url ->
                            Html.img [ src (Profile.avatar 32 url), alt "", class "size-4 shrink-0 rounded-full bg-sidebar ring-1 ring-sidebar-border", attribute "loading" "lazy" ] []

                        Nothing ->
                            span [ class "size-4 shrink-0 rounded-full bg-muted" ] []
            , span [] [ text value ]
            ]
        , Sidebar.menuBadge [ class "text-muted-foreground" ] [ text (Format.compact count) ]
        ]


languageDot : String -> String -> Html msg
languageDot language size =
    span
        [ class ("inline-block shrink-0 rounded-full " ++ size)
        , attribute "style" ("background-color: " ++ Language.color language)
        ]
        []


boolString : Bool -> String
boolString b =
    if b then
        "true"

    else
        "false"


{-| A first sync that hasn't brought any stars in yet.
-}
isFilling : Shelf -> Bool
isFilling shelf =
    Index.size shelf.index
        == 0
        && (case shelf.sync of
                Running _ ->
                    True

                Paused _ { reason } ->
                    reason == Backoff

                _ ->
                    False
           )


shelfOf : Model -> Maybe Shelf
shelfOf model =
    case model.page of
        Stars session ->
            case session.library of
                Open shelf ->
                    Just shelf

                Opening ->
                    Nothing

        Invalid _ ->
            Nothing



-- FOOTER


{-| Where syncing stands, and the way to sync again or retry.
-}
syncItem : Model -> Session -> Shelf -> DropdownMenu.Item Msg
syncItem model session shelf =
    let
        ( label, spinning, msg ) =
            case ( session.account, shelf.sync ) of
                ( _, Running run ) ->
                    ( case ( run.job, Sync.progress run.job ) of
                        ( _, Just fraction ) ->
                            "Syncing " ++ String.fromInt (round (fraction * 100)) ++ "%"

                        _ ->
                            "Syncing…"
                    , True
                    , Nothing
                    )

                ( _, Paused _ { until, reason } ) ->
                    case reason of
                        Limited ->
                            ( "Paused until " ++ Format.clock model.zone until, False, Just RetryRequested )

                        Backoff ->
                            ( "Retrying…", True, Just RetryRequested )

                ( _, Stalled _ _ ) ->
                    ( "Sync failed", False, Just RetryRequested )

                ( _, Idle ) ->
                    ( case Sync.syncedAt shelf.cursor of
                        Just at ->
                            case Format.ago model.now at of
                                "now" ->
                                    "Synced just now"

                                ago ->
                                    "Synced " ++ ago ++ " ago"

                        Nothing ->
                            "Not synced"
                    , False
                    , Just (SyncRequested Sync.Refresh)
                    )
    in
    DropdownMenu.item (attribute "aria-busy" (boolString spinning) :: (Maybe.map onClick msg |> Maybe.map List.singleton |> Maybe.withDefault []))
        [ Icon.view
            (if spinning then
                "animate-spin [animation-duration:1.5s]"

             else
                ""
            )
            Icon.refresh
        , span [ class "tabular-nums" ] [ text label ]
        ]
        |> (case msg of
                Just _ ->
                    identity

                Nothing ->
                    DropdownMenu.disabled
           )


{-| Light, dark or the system's: three icons in one rounded frame, the current one filled.
-}
themeSwitch : Model -> Html Msg
themeSwitch model =
    ToggleGroup.new
        |> ToggleGroup.onToggle ThemeSelected
        |> ToggleGroup.pressed [ model.theme ]
        |> ToggleGroup.small
        |> ToggleGroup.spacing 1
        |> ToggleGroup.items
            [ ToggleGroup.item Interop.Light [ Icon.view "" Icon.sun ] |> ToggleGroup.label "Light"
            , ToggleGroup.item Interop.Dark [ Icon.view "" Icon.moon ] |> ToggleGroup.label "Dark"
            , ToggleGroup.item Interop.System [ Icon.view "" Icon.monitor ] |> ToggleGroup.label "System"
            ]
        |> ToggleGroup.toHtml
            [ class "w-full rounded-lg border p-0.5 *:h-7 *:min-w-0 *:flex-1 *:rounded-md *:px-0 *:text-muted-foreground *:aria-pressed:bg-accent *:aria-pressed:text-foreground"
            , attribute "aria-label" "Theme"
            ]



-- TOP BAR


viewTopBar : Model -> Html Msg
viewTopBar model =
    Html.header [ class "sticky top-0 z-20 border-b bg-background/85 backdrop-blur-md supports-[backdrop-filter]:bg-background/70" ]
        -- From md up, three columns: the toggle, the search as wide as the results and centred
        -- like them, and an empty column as wide as the first to keep it there.
        [ div [ class "flex h-14 w-full items-center gap-2 px-3 md:grid md:grid-cols-[1fr_minmax(0,46rem)_1fr] md:px-4" ]
            [ div [ class "flex items-center gap-2 self-stretch" ]
                [ Sidebar.trigger [ class "-ms-1 size-9 md:size-8" ] model.sidebar SidebarChanged
                , Separator.new
                    |> Separator.vertical
                    |> Separator.toHtml [ class "me-2" ]
                ]
            , viewSearch model
            ]
        , viewProgress model
        ]


viewSearch : Model -> Html Msg
viewSearch model =
    let
        shelf =
            shelfOf model

        hint =
            case shelf of
                Just s ->
                    if Index.size s.index > 0 then
                        "Search " ++ Format.thousands (Index.size s.index) ++ " stars"

                    else
                        "Search stars"

                Nothing ->
                    "Search stars"
    in
    InputGroup.view [ class "h-9 flex-1 bg-background md:h-8" ]
        [ InputGroup.input
            [ id "search"
            , type_ "text"
            , placeholder hint
            , value model.query
            , onInput QueryChanged
            , Html.Events.on "keydown" (escapeKey QueryCleared)
            , autocomplete False
            , spellcheck False
            , attribute "autocapitalize" "off"
            , attribute "enterkeyhint" "search"
            , attribute "aria-label" "Search stars"
            ]
        , InputGroup.start [] [ Icon.view "" Icon.search ]
        , InputGroup.end []
            (List.filterMap identity
                [ if String.isEmpty model.query then
                    Just
                        (Kbd.new
                            |> Kbd.key "/"
                            |> Kbd.toHtml [ class "max-md:hidden" ]
                        )

                  else
                    Just
                        (InputGroup.button
                            |> Button.icon (Icon.view "" Icon.close)
                            |> Button.label "Clear search"
                            |> Button.toHtml [ onClick QueryCleared ]
                        )
                ]
            )
        ]


escapeKey : Msg -> Decoder Msg
escapeKey msg =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\key ->
                if key == "Escape" then
                    Decode.succeed msg

                else
                    Decode.fail "not escape"
            )


{-| How the results are ordered: one of four, the current one marked.
-}
viewOrder : Model -> Html Msg
viewOrder model =
    let
        option attrs isActive children =
            Sidebar.menuItem []
                [ Sidebar.menuButton
                    (attribute "aria-pressed" (boolString isActive)
                        :: (if isActive then
                                [ Sidebar.active ]

                            else
                                []
                           )
                        ++ attrs
                    )
                    children
                ]

        byKey key =
            case model.sort of
                Sorted current direction ->
                    if current == key then
                        -- Picking the current order again turns it around.
                        option [ onClick (SortChosen (Sorted key (Filters.flip direction))), attribute "aria-label" (Filters.keyLabel key ++ ", " ++ directionName direction) ]
                            True
                            [ span [] [ text (Filters.keyLabel key) ]
                            , Icon.view "ms-auto"
                                (case direction of
                                    Descending ->
                                        Icon.arrowDown

                                    Ascending ->
                                        Icon.arrowUp
                                )
                            ]

                    else
                        option [ onClick (SortChosen (Sorted key Descending)) ] False [ span [] [ text (Filters.keyLabel key) ] ]

                Relevance ->
                    option [ onClick (SortChosen (Sorted key Descending)) ] False [ span [] [ text (Filters.keyLabel key) ] ]
    in
    Sidebar.group []
        [ Sidebar.groupLabel [] [ sectionTitle Icon.sort "Sort by" ]
        , Sidebar.menu []
            (option [ onClick (SortChosen Relevance) ] (model.sort == Relevance) [ span [] [ text "Best match" ] ]
                :: List.map byKey Filters.keys
            )
        ]


directionName : Direction -> String
directionName direction =
    case direction of
        Descending ->
            "descending"

        Ascending ->
            "ascending"


{-| A hairline along the bottom of the bar while stars sync: how far through the pages, when
GitHub said how many there are.
-}
viewProgress : Model -> Html Msg
viewProgress model =
    let
        running =
            shelfOf model
                |> Maybe.andThen
                    (\shelf ->
                        case shelf.sync of
                            Running run ->
                                Just run

                            Paused run { reason } ->
                                if reason == Backoff then
                                    Just run

                                else
                                    Nothing

                            _ ->
                                Nothing
                    )
    in
    case running of
        Just run ->
            case Sync.progress run.job of
                Just fraction ->
                    Progress.new (max 4 (fraction * 100))
                        |> Progress.toHtml [ class progressClass, attribute "aria-label" "Syncing stars" ]

                Nothing ->
                    Progress.new 30
                        |> Progress.toHtml [ class (progressClass ++ " progress-indeterminate"), attribute "aria-label" "Syncing stars" ]

        Nothing ->
            text ""


progressClass : String
progressClass =
    "absolute inset-x-0 -bottom-px gap-0 [&_[data-slot=progress-track]]:h-0.5 [&_[data-slot=progress-track]]:rounded-none [&_[data-slot=progress-track]]:bg-transparent [&_[data-slot=progress-indicator]]:duration-500"



-- MAIN


viewMain : Model -> Html Msg
viewMain model =
    case model.page of
        Invalid segment ->
            Empty.new
                |> Empty.icon (Icon.view "" Icon.userX)
                |> Empty.title [ text "Not a GitHub username" ]
                |> Empty.description [ text ("“" ++ segment ++ "” can't be a GitHub user.") ]
                |> Empty.toHtml [ class "mt-16" ]

        Stars session ->
            case ( session.account, session.library ) of
                ( Missing, _ ) ->
                    Empty.new
                        |> Empty.icon (Icon.view "" Icon.userX)
                        |> Empty.title [ text ("No user named @" ++ Login.toString session.login) ]
                                |> Empty.toHtml [ class "mt-16" ]

                ( _, Opening ) ->
                    skeletonList 10

                ( _, Open shelf ) ->
                    viewShelf model session shelf


retryButton : Html Msg
retryButton =
    Button.new
        |> Button.text "Retry now"
        |> Button.outline
        |> Button.toHtml [ onClick RetryRequested ]


viewShelf : Model -> Session -> Shelf -> Html Msg
viewShelf model session shelf =
    if Index.size shelf.index == 0 then
        case shelf.sync of
            Paused _ { until, reason } ->
                if reason == Limited then
                    Empty.new
                        |> Empty.icon (Icon.view "" Icon.clock)
                        |> Empty.title [ text ("GitHub's limit resets at " ++ Format.clock model.zone until) ]
                        |> Empty.description [ text "Stars sync on their own after that." ]
                        |> Empty.content [ retryButton ]
                        |> Empty.toHtml [ class "mt-16" ]

                else
                    skeletonList 10

            Stalled _ _ ->
                Empty.new
                    |> Empty.icon (Icon.view "" Icon.refresh)
                    |> Empty.title [ text "Couldn't reach GitHub" ]
                    |> Empty.content [ retryButton ]
                    |> Empty.toHtml [ class "mt-16" ]

            Idle ->
                Empty.new
                    |> Empty.icon (Icon.view "" Icon.star)
                    |> Empty.title [ text ("@" ++ Login.toString session.login ++ " hasn't starred anything") ]
                        |> Empty.toHtml [ class "mt-16" ]

            _ ->
                skeletonList 10

    else if shelf.results.matches == 0 then
        Empty.new
            |> Empty.icon (Icon.view "" Icon.searchX)
            |> Empty.title [ text "No matches" ]
            |> Empty.content
                [ if Filters.isInitial model.filters then
                    Button.new
                        |> Button.text "Clear search"
                        |> Button.outline
                        |> Button.toHtml [ onClick QueryCleared ]

                  else
                    Button.new
                        |> Button.text "Reset filters"
                        |> Button.outline
                        |> Button.toHtml [ onClick FiltersReset ]
                ]
            |> Empty.toHtml [ class "mt-16" ]

    else
        viewHits model shelf


viewHits : Model -> Shelf -> Html Msg
viewHits model shelf =
        Html.Keyed.node "ul"
            [ class "flex flex-col gap-0.5", attribute "aria-label" "Starred repositories" ]
            (List.map
                (\repo ->
                    ( String.fromInt repo.id
                    , Html.Lazy.lazy5 viewHit (nowBucket model) model.sort model.query (Language.color (Maybe.withDefault "" repo.language)) repo
                    )
                )
                shelf.results.hits
                ++ (if shelf.results.matches > List.length shelf.results.hits then
                        [ ( "more-" ++ String.fromInt model.limit
                          , Html.li [ class "flex flex-col gap-0.5" ]
                                (Html.node "in-view" [ Html.Events.on "visible" (Decode.succeed MoreRequested) ] []
                                    :: List.repeat 3 skeletonHit
                                )
                          )
                        ]

                    else
                        []
                   )
            )


{-| Relative times only change by the minute, so rows stay cached between ticks.
-}
nowBucket : Model -> Int
nowBucket model =
    Time.posixToMillis model.now // 60000 * 60000


viewHit : Int -> Sort -> String -> String -> Repo -> Html Msg
viewHit now sort query color repo =
    let
        words =
            Tokens.words query

        dateLabel =
            case sort of
                Sorted StarredAt _ ->
                    "Starred " ++ Format.ago (Time.millisToPosix now) repo.starredAt ++ " ago"

                _ ->
                    "Updated " ++ Format.ago (Time.millisToPosix now) repo.pushedAt ++ " ago"
    in
    Html.li [ class "[contain-intrinsic-size:auto_5.5rem] [content-visibility:auto]" ]
        [ Item.new
            |> Item.media
                [ Avatar.new
                    |> Avatar.fallback [ text (String.toUpper (String.left 1 repo.owner)) ]
                    |> Avatar.img [ src (Profile.avatar 64 repo.avatar), alt "", class "bg-background transition-colors duration-100 group-hover/item:bg-muted", attribute "loading" "lazy", attribute "decoding" "async" ]
                    |> Avatar.toHtml [ class "size-8", attribute "aria-hidden" "true" ]
                ]
            |> Item.title
                [ span [ class "min-w-0 truncate" ]
                    [ span [ class "font-normal text-muted-foreground" ] (segments words repo.owner ++ [ text "/" ])
                    , span [] (segments words repo.name)
                    ]
                , if repo.lifecycle == Archived then
                    Badge.new |> Badge.text "Archived" |> Badge.outline |> Badge.toHtml [ class "shrink-0 text-amber-700 dark:text-amber-400" ]

                  else
                    text ""
                , if repo.origin == Fork then
                    Badge.new |> Badge.text "Fork" |> Badge.outline |> Badge.toHtml [ class "shrink-0" ]

                  else
                    text ""
                ]
            |> (case repo.description of
                    Just description ->
                        Item.description (segments words description)

                    Nothing ->
                        identity
               )
            |> Item.footer
                [ div [ class "flex min-w-0 items-center gap-3 ps-10.5 text-xs text-muted-foreground tabular-nums" ]
                    [ case repo.language of
                        Just language ->
                            span [ class "flex items-center gap-1.5" ]
                                [ span [ class "size-2 rounded-full", attribute "style" ("background-color: " ++ color) ] []
                                , text language
                                ]

                        Nothing ->
                            text ""
                    , span [ class "flex items-center gap-1" ] [ Icon.view "size-3.5" Icon.star, text (Format.compact repo.stars) ]
                    , if repo.forks > 0 then
                        span [ class "hidden items-center gap-1 sm:flex" ] [ Icon.view "size-3.5" Icon.fork, text (Format.compact repo.forks) ]

                      else
                        text ""
                    , span [ class "truncate" ] [ text dateLabel ]
                    , viewTopics words repo.topics
                    ]
                ]
            |> Item.toAnchor
                [ href (Repo.url repo)
                , target "_blank"
                , rel "noopener"
                , class "gap-x-2.5 gap-y-1.5 px-2.5 py-3 md:px-3"
                , class "[&>[data-slot=item-content]]:min-w-0 [&_[data-slot=item-title]]:w-auto [&_[data-slot=item-title]]:max-w-full"
                ]
        ]


{-| The topics a query matched, which explain hits that aren't in the name or description.
-}
viewTopics : List String -> List String -> Html msg
viewTopics words topics =
    case List.filter (\topic -> List.any (\word -> String.contains word (String.toLower topic)) words) topics of
        [] ->
            text ""

        matched ->
            span [ class "hidden min-w-0 gap-1 overflow-hidden sm:flex" ]
                (List.map
                    (\topic -> span [ class "shrink-0 rounded-md bg-muted px-1.5 py-px" ] (segments words topic))
                    (List.take 3 matched)
                )


segments : List String -> String -> List (Html msg)
segments words content =
    List.map
        (\segment ->
            case segment of
                Tokens.Plain plain ->
                    text plain

                Tokens.Match match ->
                    Html.mark [ class "rounded-[3px] bg-yellow-200/70 text-inherit dark:bg-yellow-400/25" ] [ text match ]
        )
        (Tokens.highlight words content)


skeletonList : Int -> Html msg
skeletonList count =
    Html.ul [ class "flex flex-col gap-0.5", attribute "aria-busy" "true", attribute "aria-label" "Loading stars" ]
        (List.repeat count skeletonHit)


{-| The same item, with the same paddings, as a real row: content doesn't move when it lands.
-}
skeletonHit : Html msg
skeletonHit =
    Html.li []
        [ Item.new
            |> Item.media [ Skeleton.view [ class "size-8 rounded-full!" ] ]
            |> Item.title [ span [ class "flex h-[1.375rem] items-center" ] [ Skeleton.view [ class "h-3.5 w-48 max-w-full" ] ] ]
            |> Item.description
                [ span [ class "flex h-5 items-center" ] [ Skeleton.view [ class "h-3 w-full" ] ]
                , span [ class "flex h-5 items-center" ] [ Skeleton.view [ class "h-3 w-2/3" ] ]
                ]
            |> Item.footer
                [ div [ class "flex h-4 items-center gap-3 ps-10.5" ]
                    [ Skeleton.view [ class "h-3 w-14" ]
                    , Skeleton.view [ class "h-3 w-10" ]
                    , Skeleton.view [ class "h-3 w-24" ]
                    ]
                ]
            |> Item.toHtml [ class "gap-x-2.5 gap-y-1.5 px-2.5 py-3 md:px-3" ]
        ]



-- BROWSE
