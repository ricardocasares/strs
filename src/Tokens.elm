module Tokens exposing (words, Segment(..), highlight)

{-| Splitting text into searchable words, and marking where a query matched.

Words break at anything that isn't a letter or a digit, and inside a word at camel-case humps
and between letters and digits, so `ElmUI`, `elm-ui` and `elm_ui` all answer `elm` and `ui`.
A word made of several humps also answers to all of them joined, so `elmui` finds them too.

-}


type Piece
    = Gap String
    | Word (List String)


type alias Scan =
    { pieces : List Piece
    , gap : List Char
    , humps : List String
    , hump : List Char
    , previous : Char
    }


isWordChar : Char -> Bool
isWordChar c =
    let
        code =
            Char.toCode c
    in
    Char.isAlphaNum c
        || (code > 0xBF && code < 0x2000)
        || (code >= 0x3040 && code < 0xD800)


isBoundary : Char -> Char -> Bool
isBoundary previous c =
    (Char.isLower previous && Char.isUpper c)
        || (Char.isAlpha previous && Char.isDigit c)
        || (Char.isDigit previous && Char.isAlpha c)


scan : String -> List Piece
scan text =
    String.foldl step { pieces = [], gap = [], humps = [], hump = [], previous = ' ' } text
        |> flushWord
        |> flushGap
        |> .pieces
        |> List.reverse


step : Char -> Scan -> Scan
step c state =
    if isWordChar c then
        let
            started =
                flushGap state
        in
        if not (List.isEmpty started.hump) && isBoundary state.previous c then
            { started | humps = String.fromList (List.reverse started.hump) :: started.humps, hump = [ c ], previous = c }

        else
            { started | hump = c :: started.hump, previous = c }

    else
        let
            ended =
                flushWord state
        in
        { ended | gap = c :: ended.gap, previous = c }


flushGap : Scan -> Scan
flushGap state =
    if List.isEmpty state.gap then
        state

    else
        { state | pieces = Gap (String.fromList (List.reverse state.gap)) :: state.pieces, gap = [] }


flushWord : Scan -> Scan
flushWord state =
    if List.isEmpty state.hump then
        state

    else
        { state
            | pieces = Word (List.reverse (String.fromList (List.reverse state.hump) :: state.humps)) :: state.pieces
            , humps = []
            , hump = []
        }


{-| The lower-case words of a text, for the index and for queries.
-}
words : String -> List String
words text =
    List.concatMap
        (\piece ->
            case piece of
                Gap _ ->
                    []

                Word [ single ] ->
                    [ String.toLower single ]

                Word humps ->
                    String.toLower (String.concat humps) :: List.map String.toLower humps
        )
        (scan text)


type Segment
    = Plain String
    | Match String


{-| Marks the start of each word, or hump, that a query word is a prefix of.

    highlight [ "elm" ] "Elm UI" == [ Match "Elm", Plain " UI" ]

-}
highlight : List String -> String -> List Segment
highlight queryWords text =
    if List.isEmpty queryWords then
        [ Plain text ]

    else
        scan text
            |> List.concatMap (highlightPiece queryWords)
            |> merge


highlightPiece : List String -> Piece -> List Segment
highlightPiece queryWords piece =
    case piece of
        Gap gap ->
            [ Plain gap ]

        Word humps ->
            let
                whole =
                    String.concat humps

                firstLength =
                    List.head humps |> Maybe.map String.length |> Maybe.withDefault 0
            in
            case prefixOf queryWords (String.toLower whole) of
                Just length ->
                    if length > firstLength then
                        mark length whole

                    else
                        List.concatMap (highlightHump queryWords) humps

                Nothing ->
                    List.concatMap (highlightHump queryWords) humps


highlightHump : List String -> String -> List Segment
highlightHump queryWords hump =
    case prefixOf queryWords (String.toLower hump) of
        Just length ->
            mark length hump

        Nothing ->
            [ Plain hump ]


{-| The length of the longest query word that the word starts with.
-}
prefixOf : List String -> String -> Maybe Int
prefixOf queryWords word =
    List.foldl
        (\query best ->
            if String.startsWith query word && String.length query > Maybe.withDefault 0 best then
                Just (String.length query)

            else
                best
        )
        Nothing
        queryWords


mark : Int -> String -> List Segment
mark length text =
    let
        rest =
            String.dropLeft length text
    in
    if String.isEmpty rest then
        [ Match text ]

    else
        [ Match (String.left length text), Plain rest ]


merge : List Segment -> List Segment
merge segments =
    List.foldr
        (\segment acc ->
            case ( segment, acc ) of
                ( Plain a, (Plain b) :: rest ) ->
                    Plain (a ++ b) :: rest

                ( Match a, (Match b) :: rest ) ->
                    Match (a ++ b) :: rest

                _ ->
                    segment :: acc
        )
        []
        segments
