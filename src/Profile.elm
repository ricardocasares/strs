module Profile exposing (Profile, decoder, storedDecoder, encode, displayName, avatar, isStale)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Time


{-| What `/users/{login}` says about a user, and when it was asked. The `etag` makes the
next refresh cheap when nothing changed.
-}
type alias Profile =
    { login : String
    , name : Maybe String
    , avatar : String
    , bio : Maybe String
    , followers : Int
    , fetchedAt : Time.Posix
    , etag : Maybe String
    }


{-| GitHub's payload. The time and ETag come from the response around it.
-}
decoder : Decoder (Time.Posix -> Maybe String -> Profile)
decoder =
    Decode.map5 Profile
        (Decode.field "login" Decode.string)
        (Decode.field "name" (Decode.nullable Decode.string))
        (Decode.field "avatar_url" Decode.string)
        (Decode.field "bio" (Decode.nullable Decode.string))
        (Decode.field "followers" Decode.int)


storedDecoder : Decoder Profile
storedDecoder =
    Decode.map7 Profile
        (Decode.field "login" Decode.string)
        (Decode.field "name" (Decode.nullable Decode.string))
        (Decode.field "avatar" Decode.string)
        (Decode.field "bio" (Decode.nullable Decode.string))
        (Decode.field "followers" Decode.int)
        (Decode.field "fetchedAt" (Decode.map Time.millisToPosix Decode.int))
        (Decode.field "etag" (Decode.nullable Decode.string))


encode : Profile -> Encode.Value
encode profile =
    Encode.object
        [ ( "login", Encode.string profile.login )
        , ( "name", maybe Encode.string profile.name )
        , ( "avatar", Encode.string profile.avatar )
        , ( "bio", maybe Encode.string profile.bio )
        , ( "followers", Encode.int profile.followers )
        , ( "fetchedAt", Encode.int (Time.posixToMillis profile.fetchedAt) )
        , ( "etag", maybe Encode.string profile.etag )
        ]


displayName : Profile -> String
displayName profile =
    case profile.name of
        Just name ->
            if String.isEmpty (String.trim name) then
                profile.login

            else
                name

        Nothing ->
            profile.login


{-| The avatar at a given pixel size, for sharp images without downloading the original.
-}
avatar : Int -> String -> String
avatar size url =
    url
        ++ (if String.contains "?" url then
                "&"

            else
                "?"
           )
        ++ "s="
        ++ String.fromInt size


{-| Profiles change rarely: once a day is plenty.
-}
isStale : Time.Posix -> Profile -> Bool
isStale now profile =
    Time.posixToMillis now - Time.posixToMillis profile.fetchedAt > 24 * 60 * 60 * 1000


maybe : (a -> Encode.Value) -> Maybe a -> Encode.Value
maybe encoder =
    Maybe.map encoder >> Maybe.withDefault Encode.null
