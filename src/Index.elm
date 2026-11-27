module Index exposing
    ( Index, empty, fromList
    , upsert, remove
    , size, get, ids, toList, avatar
    , score
    )

{-| A BM25F full-text index over starred repositories.

Each field is length-normalised on its own and weighted before the term frequency saturates,
so a word in a repo's name outranks the same word buried in a long description. Every query
word must match, exactly or as the start of a longer word, which is what narrowing a list as
you type expects.

Field averages move as repos come in. `upsert` normalises with the averages of the moment,
which is close enough while a sync streams pages in; `fromList` rebuilds exactly.

-}

import Dict exposing (Dict)
import Repo exposing (Repo)
import Set
import Tokens


type Index
    = Index
        { entries : Dict Int Entry
        , postings : Dict String Posting
        , totals : Fields Float
        , count : Int

        -- The terms in order, for finding the ones a typed word starts with by bisection
        -- instead of walking them all. Rebuilt once per batch of changes.
        , vocabulary : Vocabulary
        , avatars : Dict String String
        }


{-| The terms by their position in sorted order. A `Dict` rather than an `Array`: Elm's
debugger walks arrays and lists in the model one stack frame per element, and fifteen
thousand terms overflow the stack, where it takes a `Dict` whole.
-}
type alias Vocabulary =
    { terms : Dict Int String
    , size : Int
    }


type alias Entry =
    { repo : Repo
    , terms : List String
    , lengths : Fields Float
    , name : String
    }


type alias Posting =
    { count : Int
    , docs : Dict Int Float
    }


type alias Fields a =
    { name : a
    , owner : a
    , topics : a
    , description : a
    , language : a
    }


type alias Weighting =
    { weight : Float, b : Float }


weighting : Fields Weighting
weighting =
    { name = { weight = 4, b = 0.4 }
    , owner = { weight = 1.5, b = 0.4 }
    , topics = { weight = 2, b = 0.6 }
    , description = { weight = 1, b = 0.75 }
    , language = { weight = 0.75, b = 0 }
    }


k1 : Float
k1 =
    1.2


prefixWeight : Float
prefixWeight =
    0.6


maxExpansions : Int
maxExpansions =
    200



-- BUILD


empty : Index
empty =
    Index
        { entries = Dict.empty
        , postings = Dict.empty
        , totals = zeroes
        , count = 0
        , vocabulary = { terms = Dict.empty, size = 0 }
        , avatars = Dict.empty
        }


zeroes : Fields Float
zeroes =
    { name = 0, owner = 0, topics = 0, description = 0, language = 0 }


fromList : List Repo -> Index
fromList repos =
    let
        tokenized =
            List.map (\repo -> ( repo, tokenize repo )) repos

        totals =
            List.foldl (\( _, fields ) sum -> addFields (lengthsOf fields) sum) zeroes tokenized

        count =
            List.length repos
    in
    List.foldl (\( repo, fields ) index -> insert (averages totals count) repo fields index)
        (Index { entries = Dict.empty, postings = Dict.empty, totals = totals, count = count, vocabulary = { terms = Dict.empty, size = 0 }, avatars = Dict.empty })
        tokenized
        |> withVocabulary


upsert : List Repo -> Index -> Index
upsert repos index =
    List.foldl
        (\repo acc ->
            let
                (Index removed) =
                    removeOne repo.id acc

                fields =
                    tokenize repo

                lengths =
                    lengthsOf fields

                totals =
                    addFields lengths removed.totals

                withRepo =
                    Index { removed | totals = totals, count = removed.count + 1 }
            in
            insert (averages totals (removed.count + 1)) repo fields withRepo
        )
        index
        repos
        |> withVocabulary


remove : List Int -> Index -> Index
remove repoIds index =
    List.foldl removeOne index repoIds
        |> withVocabulary


removeOne : Int -> Index -> Index
removeOne id (Index data) =
    case Dict.get id data.entries of
        Just entry ->
            Index
                { data
                    | entries = Dict.remove id data.entries
                    , postings = List.foldl (removePosting id) data.postings entry.terms
                    , totals = subtractFields entry.lengths data.totals
                    , count = data.count - 1
                }

        Nothing ->
            Index data


withVocabulary : Index -> Index
withVocabulary (Index data) =
    let
        terms =
            Dict.keys data.postings
    in
    Index { data | vocabulary = { terms = Dict.fromList (List.indexedMap Tuple.pair terms), size = List.length terms } }


removePosting : Int -> String -> Dict String Posting -> Dict String Posting
removePosting id term =
    Dict.update term
        (Maybe.andThen
            (\posting ->
                if posting.count <= 1 then
                    Nothing

                else
                    Just { count = posting.count - 1, docs = Dict.remove id posting.docs }
            )
        )


{-| Adds a repo that isn't in the index, counted in the totals already.
-}
insert : Fields Float -> Repo -> Fields (List String) -> Index -> Index
insert average repo fields (Index data) =
    let
        frequencies =
            Dict.empty
                |> addField weighting.name average.name fields.name
                |> addField weighting.owner average.owner fields.owner
                |> addField weighting.topics average.topics fields.topics
                |> addField weighting.description average.description fields.description
                |> addField weighting.language average.language fields.language

        postings =
            Dict.foldl
                (\term frequency acc ->
                    Dict.update term
                        (\existing ->
                            case existing of
                                Just posting ->
                                    Just { count = posting.count + 1, docs = Dict.insert repo.id frequency posting.docs }

                                Nothing ->
                                    Just { count = 1, docs = Dict.singleton repo.id frequency }
                        )
                        acc
                )
                data.postings
                frequencies
    in
    Index
        { data
            | entries =
                Dict.insert repo.id
                    { repo = repo
                    , terms = Dict.keys frequencies
                    , lengths = lengthsOf fields
                    , name = String.toLower repo.name
                    }
                    data.entries
            , postings = postings
            , avatars = Dict.insert repo.owner repo.avatar data.avatars
        }


addField : Weighting -> Float -> List String -> Dict String Float -> Dict String Float
addField { weight, b } average tokens frequencies =
    let
        norm =
            1 - b + b * toFloat (List.length tokens) / max 1 average

        increment =
            weight / norm
    in
    List.foldl
        (\token acc -> Dict.update token (\current -> Just (Maybe.withDefault 0 current + increment)) acc)
        frequencies
        tokens


tokenize : Repo -> Fields (List String)
tokenize repo =
    { name = nameWords repo.name
    , owner = nameWords repo.owner
    , topics = List.concatMap Tokens.words repo.topics
    , description = Maybe.withDefault [] (Maybe.map Tokens.words repo.description)
    , language = Maybe.withDefault [] (Maybe.map Tokens.words repo.language)
    }


{-| `elm-ui` also answers `elmui`.
-}
nameWords : String -> List String
nameWords name =
    let
        parts =
            Tokens.words name

        whole =
            String.toLower (String.filter Char.isAlphaNum name)
    in
    if List.length parts > 1 && not (String.isEmpty whole) && not (List.member whole parts) then
        whole :: parts

    else
        parts


lengthsOf : Fields (List a) -> Fields Float
lengthsOf fields =
    { name = toFloat (List.length fields.name)
    , owner = toFloat (List.length fields.owner)
    , topics = toFloat (List.length fields.topics)
    , description = toFloat (List.length fields.description)
    , language = toFloat (List.length fields.language)
    }


addFields : Fields Float -> Fields Float -> Fields Float
addFields a b =
    { name = a.name + b.name
    , owner = a.owner + b.owner
    , topics = a.topics + b.topics
    , description = a.description + b.description
    , language = a.language + b.language
    }


subtractFields : Fields Float -> Fields Float -> Fields Float
subtractFields a b =
    addFields (scale -1 a) b


scale : Float -> Fields Float -> Fields Float
scale factor a =
    { name = factor * a.name
    , owner = factor * a.owner
    , topics = factor * a.topics
    , description = factor * a.description
    , language = factor * a.language
    }


averages : Fields Float -> Int -> Fields Float
averages totals count =
    scale (1 / toFloat (max 1 count)) totals



-- READ


size : Index -> Int
size (Index data) =
    data.count


get : Int -> Index -> Maybe Repo
get id (Index data) =
    Dict.get id data.entries |> Maybe.map .repo


ids : Index -> List Int
ids (Index data) =
    Dict.keys data.entries


{-| The avatar of an owner of any starred repo.
-}
avatar : String -> Index -> Maybe String
avatar owner (Index data) =
    Dict.get owner data.avatars


toList : Index -> List Repo
toList (Index data) =
    Dict.foldr (\_ entry acc -> entry.repo :: acc) [] data.entries



-- SEARCH


{-| The relevance of every repo that matches all the words of the query, or `Nothing` when
the query has no words, and so doesn't narrow anything.
-}
score : String -> Index -> Maybe (Dict Int Float)
score query (Index data) =
    case unique (Tokens.words query) of
        [] ->
            Nothing

        first :: rest ->
            let
                phrase =
                    String.toLower (String.filter Char.isAlphaNum query)

                matching =
                    List.foldl
                        (\word acc -> intersect acc (scoreWord data word))
                        (scoreWord data first)
                        rest
            in
            Just (Dict.map (boost phrase data.entries) matching)


{-| Typing a repo's name lands on it first, and a slight lean towards popular repos breaks
near-ties.
-}
boost : String -> Dict Int Entry -> Int -> Float -> Float
boost phrase entries id relevance =
    case Dict.get id entries of
        Just entry ->
            let
                name =
                    String.filter Char.isAlphaNum entry.name

                factor =
                    if name == phrase then
                        3

                    else if String.startsWith phrase name then
                        1.5

                    else
                        1
            in
            factor * relevance + 0.05 * logBase 10 (1 + toFloat entry.repo.stars)

        Nothing ->
            relevance


scoreWord : { a | postings : Dict String Posting, vocabulary : Vocabulary, count : Int } -> String -> Dict Int Float
scoreWord data word =
    expand word data
        |> List.foldl
            (\( factor, posting ) acc ->
                let
                    n =
                        toFloat data.count

                    df =
                        toFloat posting.count

                    idf =
                        logBase e (1 + (n - df + 0.5) / (df + 0.5))
                in
                Dict.foldl
                    (\id tf inner ->
                        let
                            value =
                                factor * idf * (tf * (k1 + 1)) / (tf + k1)
                        in
                        Dict.update id
                            (\best ->
                                case best of
                                    Just existing ->
                                        Just (max existing value)

                                    Nothing ->
                                        Just value
                            )
                            inner
                    )
                    acc
                    posting.docs
            )
            Dict.empty


{-| The word itself, and the longer words it starts, weighted down by how much is missing.
-}
expand : String -> { a | postings : Dict String Posting, vocabulary : Vocabulary } -> List ( Float, Posting )
expand word { postings, vocabulary } =
    let
        length =
            toFloat (String.length word)

        collect index found acc =
            case Dict.get index vocabulary.terms of
                Just term ->
                    if found < maxExpansions && String.startsWith word term then
                        let
                            weight =
                                if term == word then
                                    1

                                else
                                    prefixWeight * sqrt (length / toFloat (String.length term))
                        in
                        case Dict.get term postings of
                            Just posting ->
                                collect (index + 1) (found + 1) (( weight, posting ) :: acc)

                            Nothing ->
                                collect (index + 1) found acc

                    else
                        acc

                Nothing ->
                    acc
    in
    collect (firstAtLeast word vocabulary) 0 []


{-| The position of the first term that sorts at or after the word: where the terms it
starts begin.
-}
firstAtLeast : String -> Vocabulary -> Int
firstAtLeast word vocabulary =
    let
        bisect low high =
            if low >= high then
                low

            else
                let
                    middle =
                        (low + high) // 2
                in
                case Dict.get middle vocabulary.terms of
                    Just term ->
                        if term < word then
                            bisect (middle + 1) high

                        else
                            bisect low middle

                    Nothing ->
                        low
    in
    bisect 0 vocabulary.size


intersect : Dict Int Float -> Dict Int Float -> Dict Int Float
intersect a b =
    Dict.merge
        (\_ _ acc -> acc)
        (\id x y acc -> Dict.insert id (x + y) acc)
        (\_ _ acc -> acc)
        a
        b
        Dict.empty


unique : List String -> List String
unique =
    List.foldr
        (\word ( seen, acc ) ->
            if Set.member word seen then
                ( seen, acc )

            else
                ( Set.insert word seen, word :: acc )
        )
        ( Set.empty, [] )
        >> Tuple.second
