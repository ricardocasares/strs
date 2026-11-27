module Main exposing (Model, Msg, main)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Filters exposing (Choice(..), Filters, Order(..))
import Format
import GitHub
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (alt, attribute, autocomplete, class, disabled, href, id, placeholder, rel, spellcheck, src, target, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed
import Html.Lazy
import Icon
import Index exposing (Index)
import InteropDefinitions as Interop exposing (Theme)
import InteropPorts
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Language
import Login exposing (Login)
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
import UI.Dialog as Dialog
import UI.DropdownMenu as DropdownMenu
import UI.Empty as Empty
import UI.InputGroup as InputGroup
import UI.Item as Item
import UI.Kbd as Kbd
import UI.Progress as Progress
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
    , base : String
    , api : GitHub.Api
    , theme : Theme
    , systemDark : Bool
    , zone : Time.Zone
    , now : Time.Posix
    , blockedUntil : Maybe Time.Posix
    , sidebar : Sidebar.State
    , toaster : Toast.Toaster Msg
    , accounts : List Summary
    , browse : Browse
    , query : String
    , filters : Filters
    , order : Order
    , limit : Int
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
    , languages : List ( String, Int )
    }


{-| A user whose stars are saved in this browser.
-}
type alias Summary =
    { login : String
    , profile : Maybe Profile
    , total : Int
    , visitedAt : Time.Posix
    }


type Browse
    = Closed
    | Browsing String


pageSize : Int
pageSize =
    60


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init json url key =
    let
        flags =
            InteropPorts.decodeFlags json
                |> Result.withDefault { theme = Nothing, systemDark = False, blockedUntil = Nothing, api = "https://api.github.com", base = "/", now = 0 }

        now =
            Time.millisToPosix flags.now

        model =
            { key = key
            , base = flags.base
            , api = GitHub.api flags.api
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
            , sidebar = Sidebar.init
            , toaster = Toast.init
            , accounts = []
            , browse = Closed
            , query = ""
            , filters = Filters.initial
            , order = Relevance
            , limit = pageSize
            , page = Invalid ""
            }

        ( routed, cmd ) =
            route url model
    in
    ( routed
    , Cmd.batch
        [ cmd
        , InteropPorts.fromElm Interop.LoadAccounts
        , Task.perform GotZone Time.here
        ]
    )


{-| `/strs/` is the default user, `/strs/<login>` anyone else.
-}
route : Url -> Model -> ( Model, Cmd Msg )
route url model =
    let
        segment =
            String.dropLeft (String.length model.base) (url.path ++ "/")
                |> String.split "/"
                |> List.head
                |> Maybe.withDefault ""
                |> Url.percentDecode
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
        , browse = Closed
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

        passing =
            List.filter (Tuple.first >> Filters.matches model.filters) scored

        languages =
            List.foldl
                (\( repo, _ ) counts ->
                    case repo.language of
                        Just language ->
                            Dict.update language (Maybe.withDefault 0 >> (+) 1 >> Just) counts

                        Nothing ->
                            counts
                )
                Dict.empty
                passing

        chosen =
            if Set.isEmpty model.filters.languages then
                passing

            else
                List.filter (\( repo, _ ) -> Maybe.map (\l -> Set.member l model.filters.languages) repo.language == Just True) passing
    in
    { matches = List.length chosen
    , hits = List.sortWith (compareBy model.order) chosen |> List.take model.limit |> List.map Tuple.first
    , languages =
        Dict.toList languages
            |> List.sortWith (\( a, x ) ( b, y ) -> compareThen (compare y x) (compare a b))
    }


compareBy : Order -> ( Repo, Float ) -> ( Repo, Float ) -> Basics.Order
compareBy order ( a, x ) ( b, y ) =
    let
        recent =
            compare (Time.posixToMillis b.starredAt) (Time.posixToMillis a.starredAt)
    in
    case order of
        Relevance ->
            compareThen (compare y x) recent

        RecentlyStarred ->
            recent

        MostStars ->
            compareThen (compare b.stars a.stars) recent

        RecentlyUpdated ->
            compareThen (compare (Time.posixToMillis b.pushedAt) (Time.posixToMillis a.pushedAt)) recent


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
    | LifecycleChosen (Choice Lifecycle)
    | OriginChosen (Choice Origin)
    | StarsChanged ( Float, Float )
    | LanguageToggled String
    | LanguagesCleared
    | FiltersReset
    | OrderChosen Order
    | MoreRequested
    | SyncRequested Sync.Mode
    | RetryRequested
    | GotPage Login Sync.Job ( GitHub.Response (List Repo), Time.Posix )
    | GotProfile Login ( GitHub.Response (Time.Posix -> Profile), Time.Posix )
    | Resumed Login Time.Posix
    | VisibilityChanged Browser.Events.Visibility
    | BrowseOpened
    | BrowseClosed
    | BrowseChanged String
    | BrowseSubmitted Login
    | ProfileOpened String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked (Browser.Internal url) ->
            ( model, Nav.pushUrl model.key (Url.toString url) )

        LinkClicked (Browser.External url) ->
            ( model, Nav.load url )

        UrlChanged url ->
            route url model

        Tick now ->
            ( { model | now = now }, Cmd.none )

        GotZone zone ->
            ( { model | zone = zone }, Cmd.none )

        GotInterop (Ok message) ->
            fromJs message model

        GotInterop (Err _) ->
            ( model, Cmd.none )

        SidebarChanged sidebar ->
            ( { model | sidebar = sidebar }, Cmd.none )

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

        LanguageToggled language ->
            setFilters
                (\filters ->
                    { filters
                        | languages =
                            if Set.member language filters.languages then
                                Set.remove language filters.languages

                            else
                                Set.insert language filters.languages
                    }
                )
                model

        LanguagesCleared ->
            setFilters (\filters -> { filters | languages = Set.empty }) model

        FiltersReset ->
            setFilters (\_ -> Filters.initial) model

        OrderChosen order ->
            ( refresh { model | order = order, limit = pageSize }, Cmd.none )

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

        BrowseOpened ->
            ( { model | browse = Browsing "" }, focus "browse" )

        BrowseClosed ->
            ( { model | browse = Closed }, Cmd.none )

        BrowseChanged draft ->
            ( { model | browse = Browsing draft }, Cmd.none )

        BrowseSubmitted login ->
            ( { model | browse = Closed }, Nav.pushUrl model.key (model.base ++ Login.toString login) )

        ProfileOpened url ->
            ( model, InteropPorts.fromElm (Interop.OpenUrl url) )


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

        Interop.AccountsLoaded json ->
            ( { model | accounts = decodeAccounts json }, Cmd.none )

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


type alias Stored =
    { profile : Maybe Profile
    , cursor : Sync.Cursor
    , total : Int
    , visitedAt : Time.Posix
    }


accountDecoder : Decoder Stored
accountDecoder =
    Decode.map4 Stored
        (Decode.field "profile" (Decode.nullable Profile.storedDecoder))
        (Decode.field "cursor" Sync.cursorDecoder)
        (Decode.field "total" Decode.int)
        (Decode.field "visitedAt" (Decode.map Time.millisToPosix Decode.int))


decodeAccounts : Encode.Value -> List Summary
decodeAccounts json =
    Decode.decodeValue
        (Decode.list
            (Decode.map2 (\login stored -> { login = login, profile = stored.profile, total = stored.total, visitedAt = stored.visitedAt })
                (Decode.field "login" Decode.string)
                accountDecoder
            )
        )
        json
        |> Result.withDefault []
        |> sortAccounts


sortAccounts : List Summary -> List Summary
sortAccounts =
    List.sortBy (.visitedAt >> Time.posixToMillis >> negate)


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
            , GitHub.profile model.api session.login etag
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
            , GitHub.starred model.api session.login (Sync.request run.job)
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
                        |> andThen (\m -> ( m, InteropPorts.fromElm Interop.LoadAccounts ))

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
                , ( "total", Encode.int (Index.size shelf.index) )
                , ( "visitedAt", Encode.int (Time.posixToMillis model.now) )
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


{-| `/` or ⌘K focuses the search, unless a field already has the keyboard.
-}
shortcut : Decoder Msg
shortcut =
    Decode.map4
        (\key meta ctrl tag ->
            if List.member tag [ "INPUT", "TEXTAREA", "SELECT" ] then
                Ignored

            else if key == "/" || ((meta || ctrl) && key == "k") then
                SearchFocused

            else
                Ignored
        )
        (Decode.field "key" Decode.string)
        (Decode.field "metaKey" Decode.bool)
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.oneOf [ Decode.at [ "target", "tagName" ] Decode.string, Decode.succeed "" ])



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
                |> Sidebar.header [ viewAccount model ]
                |> Sidebar.content (viewFilters model)
                |> Sidebar.footer [ viewFooter model ]
                |> Sidebar.rail
                |> Sidebar.page [ class "min-w-0" ]
                    [ viewTopBar model
                    , Html.section [ class "mx-auto w-full max-w-3xl px-2 pt-2 pb-16 md:px-4" ] [ viewMain model ]
                    ]
                |> Sidebar.toHtml []
            , viewBrowse model
            , Toast.view ToastMsg model.toaster
            ]
        ]
    }



-- ACCOUNT


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
                |> DropdownMenu.toHtml [ class "w-(--sidebar-width) max-w-[calc(100vw-2rem)]", attribute "style" "--sidebar-width: 15rem" ]

        Invalid _ ->
            Sidebar.menuButton [ Sidebar.large, onClick BrowseOpened ]
                [ Icon.view "" Icon.userSearch, span [] [ text "Browse a user" ] ]


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
                (userAvatar profile.avatar login [ class "size-8" ])
                (span [ class "truncate text-sm font-medium" ] [ text (Profile.displayName profile) ])

        Unknown ->
            layout
                (Skeleton.view [ class "size-8 shrink-0 rounded-full!" ])
                (span [ class "flex h-5 items-center" ] [ Skeleton.view [ class "h-3.5 w-24" ] ])

        Missing ->
            layout
                (span [ class "flex size-8 shrink-0 items-center justify-center rounded-full bg-muted text-muted-foreground" ] [ Icon.view "" Icon.userX ])
                (span [ class "truncate text-sm font-medium" ] [ text login ])


userAvatar : String -> String -> List (Html.Attribute Msg) -> Html Msg
userAvatar url login attrs =
    Avatar.new
        |> Avatar.fallback [ text (String.toUpper (String.left 1 login)) ]
        |> Avatar.img [ src (Profile.avatar 80 url), alt "" ]
        |> Avatar.toHtml (attribute "aria-hidden" "true" :: attrs)


accountMenu : Model -> Session -> List (DropdownMenu.Item Msg)
accountMenu model session =
    let
        current =
            Login.toString session.login

        others =
            List.take 6 model.accounts
    in
    (if List.isEmpty others then
        []

     else
        [ DropdownMenu.group
            (List.map
                (\summary ->
                    DropdownMenu.radioItem (summary.login == current)
                        [ onClick (BrowseSubmitted (Maybe.withDefault session.login (Login.fromString summary.login))) ]
                        [ case summary.profile of
                            Just profile ->
                                userAvatar profile.avatar summary.login [ class "size-5" ]

                            Nothing ->
                                Avatar.new |> Avatar.fallback [ text (String.toUpper (String.left 1 summary.login)) ] |> Avatar.toHtml [ class "size-5 text-[10px]" ]
                        , span [ class "truncate" ] [ text summary.login ]
                        , span [ class "ms-auto text-xs text-muted-foreground tabular-nums" ] [ text (Format.compact summary.total) ]
                        ]
                )
                others
            )
        , DropdownMenu.separator
        ]
    )
        ++ [ DropdownMenu.item [ onClick BrowseOpened ] [ Icon.view "" Icon.userSearch, text "Browse user…" ]
           , DropdownMenu.item [ onClick (ProfileOpened ("https://github.com/" ++ current ++ "?tab=stars")) ] [ Icon.view "" Icon.external, text "Open on GitHub" ]
           , DropdownMenu.item [ onClick (SyncRequested Sync.Rebuild) ] [ Icon.view "" Icon.refresh, text "Resync all" ]
                |> (if isSyncing session then
                        DropdownMenu.disabled

                    else
                        identity
                   )
           ]


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


viewFilters : Model -> List (Html Msg)
viewFilters model =
    let
        filters =
            model.filters
    in
    [ Sidebar.group []
        [ Sidebar.groupLabel [] [ text "Filters" ]
        , if Filters.isInitial { filters | languages = Set.empty } then
            text ""

          else
            Sidebar.groupAction [ onClick FiltersReset, attribute "aria-label" "Reset filters" ] [ Icon.view "" Icon.reset ]
        , Sidebar.groupContent [ class "flex flex-col gap-2 px-1" ]
            [ segmented "Status"
                LifecycleChosen
                filters.lifecycle
                [ ( Any, "All" ), ( Only Active, "Active" ), ( Only Archived, "Archived" ) ]
            , segmented "Type"
                OriginChosen
                filters.origin
                [ ( Any, "All" ), ( Only Source, "Sources" ), ( Only Fork, "Forks" ) ]
            ]
        ]
    , Sidebar.group []
        [ Sidebar.groupLabel [ class "justify-between" ]
            [ text "Stars"
            , span [ class "font-normal tabular-nums text-sidebar-foreground" ] [ text (Filters.starsLabel filters.stars) ]
            ]
        , Sidebar.groupContent [ class "px-2 pt-1 pb-2" ]
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
    , viewLanguages model
    ]


segmented : String -> (Choice a -> Msg) -> Choice a -> List ( Choice a, String ) -> Html Msg
segmented label toMsg current options =
    ToggleGroup.new
        |> ToggleGroup.onToggle toMsg
        |> ToggleGroup.pressed [ current ]
        |> ToggleGroup.outline
        |> ToggleGroup.small
        |> ToggleGroup.spacing 0
        |> ToggleGroup.items (List.map (\( value, name ) -> ToggleGroup.item value [ text name ]) options)
        |> ToggleGroup.toHtml [ class "w-full *:flex-1 *:bg-background", attribute "aria-label" label ]


viewLanguages : Model -> Html Msg
viewLanguages model =
    let
        selected =
            model.filters.languages
    in
    Sidebar.group []
        [ Sidebar.groupLabel [] [ text "Languages" ]
        , if Set.isEmpty selected then
            text ""

          else
            Sidebar.groupAction [ onClick LanguagesCleared, attribute "aria-label" "Clear languages" ] [ Icon.view "" Icon.close ]
        , case shelfOf model |> Maybe.andThen (\shelf -> if isFilling shelf then Nothing else Just shelf) of
            Just shelf ->
                let
                    counted =
                        shelf.results.languages

                    missing =
                        Set.toList selected
                            |> List.filter (\language -> not (List.any (Tuple.first >> (==) language) counted))
                            |> List.map (\language -> ( language, 0 ))

                    all =
                        counted ++ missing

                    ( top, rest ) =
                        ( List.take 8 all, List.drop 8 all )
                in
                Sidebar.menu []
                    (List.map (languageItem selected) top
                        ++ (if List.isEmpty rest then
                                []

                            else
                                [ Sidebar.menuItem []
                                    [ Sidebar.collapsible
                                        (if List.any (Tuple.first >> (\l -> Set.member l selected)) rest then
                                            [ Sidebar.defaultOpen ]

                                         else
                                            []
                                        )
                                        (Sidebar.menuTrigger [ class "text-muted-foreground" ]
                                            [ span [] [ text (String.fromInt (List.length rest) ++ " more") ] ]
                                        )
                                        [ Sidebar.menu [ class "mt-1" ] (List.map (languageItem selected) rest) ]
                                    ]
                                ]
                           )
                    )

            Nothing ->
                Sidebar.menu []
                    (List.map
                        (\width -> Sidebar.menuItem [] [ Sidebar.menuSkeleton [ Sidebar.showIcon, class width ] ])
                        [ "[--skeleton-width:55%]", "[--skeleton-width:70%]", "[--skeleton-width:45%]", "[--skeleton-width:60%]", "[--skeleton-width:50%]", "[--skeleton-width:65%]" ]
                    )
        ]


languageItem : Set.Set String -> ( String, Int ) -> Html Msg
languageItem selected ( language, count ) =
    let
        isSelected =
            Set.member language selected
    in
    Sidebar.menuItem []
        [ Sidebar.menuButton
            ([ onClick (LanguageToggled language), attribute "aria-pressed" (boolString isSelected) ]
                ++ (if isSelected then
                        [ Sidebar.active ]

                    else
                        []
                   )
            )
            [ languageDot language "size-2.5"
            , span [] [ text language ]
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
    Index.size shelf.index == 0 && shelf.sync /= Idle


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


viewFooter : Model -> Html Msg
viewFooter model =
    Sidebar.menu []
        [ Sidebar.menuItem [ class "flex items-center gap-1" ]
            [ case model.page of
                Stars session ->
                    case ( session.account, session.library ) of
                        ( Missing, _ ) ->
                            span [ class "flex-1" ] []

                        ( _, Open shelf ) ->
                            syncButton model session shelf

                        ( _, Opening ) ->
                            Sidebar.menuSkeleton [ Sidebar.showIcon, class "flex-1 [--skeleton-width:60%]" ]

                Invalid _ ->
                    span [ class "flex-1" ] []
            , themeMenu model
            ]
        ]


syncButton : Model -> Session -> Shelf -> Html Msg
syncButton model session shelf =
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
                    , Ignored
                    )

                ( _, Paused _ { until, reason } ) ->
                    case reason of
                        Limited ->
                            ( "Paused until " ++ Format.clock model.zone until, False, RetryRequested )

                        Backoff ->
                            ( "Retrying…", True, RetryRequested )

                ( _, Stalled _ _ ) ->
                    ( "Sync failed", False, RetryRequested )

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
                    , SyncRequested Sync.Refresh
                    )
    in
    Sidebar.menuButton [ onClick msg, class "flex-1 text-muted-foreground", attribute "aria-busy" (boolString spinning) ]
        [ Icon.view
            (if spinning then
                "animate-spin [animation-duration:1.5s]"

             else
                ""
            )
            Icon.refresh
        , span [ class "tabular-nums" ] [ text label ]
        ]


themeMenu : Model -> Html Msg
themeMenu model =
    let
        icon theme =
            case theme of
                Interop.Light ->
                    Icon.sun

                Interop.Dark ->
                    Icon.moon

                Interop.System ->
                    Icon.monitor

        option theme name =
            DropdownMenu.radioItem (model.theme == theme) [ onClick (ThemeSelected theme) ] [ Icon.view "" (icon theme), text name ]
    in
    DropdownMenu.new
        |> DropdownMenu.id "theme"
        |> DropdownMenu.trigger
            (Button.new
                |> Button.icon (Icon.view "" (icon model.theme))
                |> Button.label "Theme"
                |> Button.ghost
                |> Button.toAttributes
                |> (++) [ attribute "aria-label" "Theme" ]
            )
            [ Icon.view "" (icon model.theme) ]
        |> DropdownMenu.content
            [ option Interop.Light "Light"
            , option Interop.Dark "Dark"
            , option Interop.System "System"
            ]
        |> DropdownMenu.top
        |> DropdownMenu.alignEnd
        |> DropdownMenu.toHtml [ class "w-36" ]



-- TOP BAR


viewTopBar : Model -> Html Msg
viewTopBar model =
    Html.header [ class "sticky top-0 z-20 border-b bg-background/85 backdrop-blur-md supports-[backdrop-filter]:bg-background/70" ]
        [ div [ class "mx-auto flex h-14 w-full max-w-3xl items-center gap-2 px-2 md:px-4" ]
            [ Sidebar.trigger [ class "size-9 md:size-8" ] model.sidebar SidebarChanged
            , viewSearch model
            , viewOrder model
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

        filtered =
            not (String.isEmpty (String.trim model.query)) || not (Filters.isInitial model.filters)
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
                [ case ( shelf, filtered ) of
                    ( Just s, True ) ->
                        Just (InputGroup.text [ class "tabular-nums text-xs" ] [ text (Format.thousands s.results.matches) ])

                    _ ->
                        Nothing
                , if String.isEmpty model.query then
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


viewOrder : Model -> Html Msg
viewOrder model =
    DropdownMenu.new
        |> DropdownMenu.id "order"
        |> DropdownMenu.trigger
            (Button.new
                |> Button.icon (Icon.view "" Icon.sort)
                |> Button.label "Sort"
                |> Button.ghost
                |> Button.toAttributes
                |> (++) [ class "size-9 md:size-8", attribute "aria-label" ("Sort: " ++ Filters.orderLabel model.order) ]
            )
            [ Icon.view "" Icon.sort ]
        |> DropdownMenu.content
            [ DropdownMenu.group
                (List.map
                    (\order -> DropdownMenu.radioItem (model.order == order) [ onClick (OrderChosen order) ] [ text (Filters.orderLabel order) ])
                    Filters.orders
                )
            ]
        |> DropdownMenu.alignEnd
        |> DropdownMenu.toHtml [ class "w-44" ]


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
                |> Empty.content [ browseButton ]
                |> Empty.toHtml [ class "mt-16" ]

        Stars session ->
            case ( session.account, session.library ) of
                ( Missing, _ ) ->
                    Empty.new
                        |> Empty.icon (Icon.view "" Icon.userX)
                        |> Empty.title [ text ("No user named @" ++ Login.toString session.login) ]
                        |> Empty.content [ browseButton ]
                        |> Empty.toHtml [ class "mt-16" ]

                ( _, Opening ) ->
                    skeletonList 10

                ( _, Open shelf ) ->
                    viewShelf model session shelf


browseButton : Html Msg
browseButton =
    Button.new
        |> Button.text "Browse another user"
        |> Button.outline
        |> Button.startIcon (Icon.view "" Icon.userSearch)
        |> Button.toHtml [ onClick BrowseOpened ]


viewShelf : Model -> Session -> Shelf -> Html Msg
viewShelf model session shelf =
    if Index.size shelf.index == 0 then
        case shelf.sync of
            Idle ->
                Empty.new
                    |> Empty.icon (Icon.view "" Icon.star)
                    |> Empty.title [ text ("@" ++ Login.toString session.login ++ " hasn't starred anything") ]
                    |> Empty.content [ browseButton ]
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
        Html.Keyed.node "ul"
            [ class "flex flex-col gap-0.5", attribute "aria-label" "Starred repositories" ]
            (List.map
                (\repo ->
                    ( String.fromInt repo.id
                    , Html.Lazy.lazy5 viewHit (nowBucket model) model.order model.query (Language.color (Maybe.withDefault "" repo.language)) repo
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


viewHit : Int -> Order -> String -> String -> Repo -> Html Msg
viewHit now order query color repo =
    let
        words =
            Tokens.words query

        dateLabel =
            case order of
                RecentlyStarred ->
                    "Starred " ++ Format.ago (Time.millisToPosix now) repo.starredAt ++ " ago"

                _ ->
                    "Updated " ++ Format.ago (Time.millisToPosix now) repo.pushedAt ++ " ago"
    in
    Html.li [ class "[contain-intrinsic-size:auto_5.5rem] [content-visibility:auto]" ]
        [ Item.new
            |> Item.media
                [ Avatar.new
                    |> Avatar.fallback [ text (String.toUpper (String.left 1 repo.owner)) ]
                    |> Avatar.img [ src (Profile.avatar 64 repo.avatar), alt "", attribute "loading" "lazy", attribute "decoding" "async" ]
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


viewBrowse : Model -> Html Msg
viewBrowse model =
    let
        draft =
            case model.browse of
                Browsing text_ ->
                    text_

                Closed ->
                    ""

        parsed =
            Login.fromString draft

        invalid =
            not (String.isEmpty (String.trim draft)) && parsed == Nothing
    in
    Dialog.new
        |> Dialog.id "browse-dialog"
        |> Dialog.open (model.browse /= Closed) BrowseClosed
        |> Dialog.title [ text "Browse stars" ]
        |> Dialog.content
            [ Html.form
                [ id "browse-form"
                , onSubmit
                    (case parsed of
                        Just login ->
                            BrowseSubmitted login

                        Nothing ->
                            Ignored
                    )
                ]
                [ InputGroup.view []
                    [ InputGroup.input
                        [ id "browse"
                        , placeholder "username"
                        , value draft
                        , onInput BrowseChanged
                        , autocomplete False
                        , spellcheck False
                        , attribute "autocapitalize" "off"
                        , attribute "aria-label" "GitHub username"
                        , attribute "aria-invalid" (boolString invalid)
                        ]
                    , InputGroup.start [] [ InputGroup.text [] [ text "@" ] ]
                    ]
                ]
            ]
        |> Dialog.footer []
            [ Button.new
                |> Button.text "View stars"
                |> Button.toHtml [ type_ "submit", attribute "form" "browse-form", disabled (parsed == Nothing) ]
            ]
        |> Dialog.toHtml []
