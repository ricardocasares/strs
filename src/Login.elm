module Login exposing (Login, fromString, toString, default)

{-| A GitHub username that has passed GitHub's own rules. Usernames are case-insensitive, so
a `Login` is kept in lower case: the same user is the same key in the URL and in storage.
-}


type Login
    = Login String


{-| One to 39 letters, digits or single hyphens, not starting or ending with a hyphen.
-}
fromString : String -> Maybe Login
fromString raw =
    let
        login =
            String.toLower (String.trim (String.replace "@" "" raw))
    in
    if
        String.length login
            >= 1
            && String.length login
            <= 39
            && String.all (\c -> Char.isAlphaNum c || c == '-') login
            && not (String.startsWith "-" login)
            && not (String.endsWith "-" login)
            && not (String.contains "--" login)
    then
        Just (Login login)

    else
        Nothing


toString : Login -> String
toString (Login login) =
    login


default : Login
default =
    Login "ricardocasares"
