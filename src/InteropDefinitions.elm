module InteropDefinitions exposing
    ( Flags, FromElm(..), ToElm(..), Theme(..), Store(..), Operation(..)
    , interop, themeToString
    )

{-| The ports. JavaScript only touches browser APIs here: IndexedDB reads and writes,
localStorage, and opening a tab. What to store, when, and what it means is decided in Elm.
-}

import Json.Encode as Encode
import TsJson.Decode as TsDecode exposing (Decoder)
import TsJson.Encode as TsEncode exposing (Encoder)


interop :
    { toElm : Decoder ToElm
    , fromElm : Encoder FromElm
    , flags : Decoder Flags
    }
interop =
    { toElm = toElm
    , fromElm = fromElm
    , flags = flags
    }


type Theme
    = Light
    | Dark
    | System


type Store
    = Repos
    | Accounts


{-| One change in a write. All the operations of a `Write` share one transaction.
-}
type Operation
    = Put Store Encode.Value
    | Delete Store Encode.Value


type FromElm
    = LoadUser String
    | LoadAccounts
    | Write (List Operation)
    | SaveSetting { key : String, value : Maybe String }
    | OpenUrl String


type ToElm
    = SystemThemeChanged Bool
    | UserLoaded { login : String, repos : Encode.Value, account : Encode.Value }
    | AccountsLoaded Encode.Value
    | StorageFailed String


type alias Flags =
    { theme : Maybe Theme
    , systemDark : Bool
    , blockedUntil : Maybe Int
    , api : String
    , base : String
    , now : Int
    , sidebarOpen : Maybe Bool
    }


themeToString : Theme -> String
themeToString theme =
    case theme of
        Light ->
            "light"

        Dark ->
            "dark"

        System ->
            "system"


storeToString : Store -> String
storeToString target =
    case target of
        Repos ->
            "repos"

        Accounts ->
            "accounts"


fromElm : Encoder FromElm
fromElm =
    TsEncode.union
        (\loadUser loadAccounts write saveSetting openUrl value ->
            case value of
                LoadUser login ->
                    loadUser login

                LoadAccounts ->
                    loadAccounts

                Write operations ->
                    write operations

                SaveSetting setting ->
                    saveSetting setting

                OpenUrl url ->
                    openUrl url
        )
        |> TsEncode.variantObject "LoadUser" [ TsEncode.required "login" identity TsEncode.string ]
        |> TsEncode.variantLiteral (Encode.object [ ( "tag", Encode.string "LoadAccounts" ) ])
        |> TsEncode.variantObject "Write" [ TsEncode.required "operations" identity (TsEncode.list operation) ]
        |> TsEncode.variantObject "SaveSetting"
            [ TsEncode.required "key" .key TsEncode.string
            , TsEncode.required "value" .value (TsEncode.maybe TsEncode.string)
            ]
        |> TsEncode.variantObject "OpenUrl" [ TsEncode.required "url" identity TsEncode.string ]
        |> TsEncode.buildUnion


operation : Encoder Operation
operation =
    TsEncode.union
        (\put delete value ->
            case value of
                Put target record ->
                    put ( target, record )

                Delete target key ->
                    delete ( target, key )
        )
        |> TsEncode.variantObject "Put"
            [ TsEncode.required "store" Tuple.first store
            , TsEncode.required "value" Tuple.second TsEncode.value
            ]
        |> TsEncode.variantObject "Delete"
            [ TsEncode.required "store" Tuple.first store
            , TsEncode.required "key" Tuple.second TsEncode.value
            ]
        |> TsEncode.buildUnion


store : Encoder Store
store =
    TsEncode.union
        (\repos accounts value ->
            case value of
                Repos ->
                    repos

                Accounts ->
                    accounts
        )
        |> TsEncode.variantLiteral (Encode.string (storeToString Repos))
        |> TsEncode.variantLiteral (Encode.string (storeToString Accounts))
        |> TsEncode.buildUnion


toElm : Decoder ToElm
toElm =
    TsDecode.discriminatedUnion "tag"
        [ ( "SystemThemeChanged", TsDecode.map SystemThemeChanged (TsDecode.field "dark" TsDecode.bool) )
        , ( "UserLoaded"
          , TsDecode.map3 (\login repos account -> UserLoaded { login = login, repos = repos, account = account })
                (TsDecode.field "login" TsDecode.string)
                (TsDecode.field "repos" TsDecode.value)
                (TsDecode.field "account" TsDecode.value)
          )
        , ( "AccountsLoaded", TsDecode.map AccountsLoaded (TsDecode.field "accounts" TsDecode.value) )
        , ( "StorageFailed", TsDecode.map StorageFailed (TsDecode.field "message" TsDecode.string) )
        ]


themeDecoder : Decoder Theme
themeDecoder =
    TsDecode.stringUnion [ ( "light", Light ), ( "dark", Dark ), ( "system", System ) ]


flags : Decoder Flags
flags =
    TsDecode.succeed Flags
        |> TsDecode.andMap (TsDecode.field "theme" (TsDecode.nullable themeDecoder))
        |> TsDecode.andMap (TsDecode.field "systemDark" TsDecode.bool)
        |> TsDecode.andMap (TsDecode.field "blockedUntil" (TsDecode.nullable TsDecode.int))
        |> TsDecode.andMap (TsDecode.field "api" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "base" TsDecode.string)
        |> TsDecode.andMap (TsDecode.field "now" TsDecode.int)
        |> TsDecode.andMap (TsDecode.field "sidebarOpen" (TsDecode.nullable TsDecode.bool))
