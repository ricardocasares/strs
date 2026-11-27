module Index exposing
    ( Index, empty, fromList
    , upsert, remove
    , pending, settle
    , size, get, ids, toList, avatar, starBounds
    , Facets, facets
    , score
    )

{-| A BM25F full-text index over starred repositories.

Each field is length-normalised on its own and weighted before the term frequency saturates,
so a word in a repo's name outranks the same word buried in a long description. Every query
word must match, exactly or as the start of a longer word, which is what narrowing a list as
you type expects.

The index keeps each term's raw count per field, and normalises by field length when a query
is scored, against the averages of the whole library at that moment. So adding or removing
repos never leaves the others normalised against averages that have since moved, and nothing
ever needs rebuilding.

Repos join the library at once, so lists and filters see them straight away, but they are
indexed for search a batch at a time with `settle`. That keeps a page of stars, or a whole
library read from the database, from holding the main thread while it's tokenised. `score`
settles whatever is left first, so a search never misses a repo.

-}

import Dict exposing (Dict)
import Filters
import Repo exposing (Repo)
import Set exposing (Set)
import Tokens


type Index
    = Index
        { entries : Dict Int Entry
        , count : Int

        -- The repos not indexed for search yet.
        , queue : Set Int
        , postings : Dict String Posting

        -- The field lengths of the indexed repos, and how many there are.
        , totals : Fields Float
        , indexed : Int

        -- The terms in order, for finding the ones a typed word starts with by bisection
        -- instead of walking them all. Covers every term whenever the queue is empty; terms
        -- whose last repo was removed may linger, and are skipped.
        , vocabulary : Vocabulary
        , avatars : Dict String String
        , facets : Facets
        }


{-| How many repos in the library have each language, topic and owner.
-}
type alias Facets =
    { languages : Dict String Int
    , topics : Dict String Int
    , owners : Dict String Int
    }


{-| The terms by their position in sorted order. A `Dict` rather than an `Array`: Elm's
debugger walks arrays and lists in the model one stack frame per element, and fifteen
thousand terms overflow the stack, where it takes a `Dict` whole.
-}
type alias Vocabulary =
    { terms : Dict Int String
    , size : Int
    }


type Entry
    = Queued Repo
    | Indexed Repo Words


{-| What indexing a repo took note of: the terms to take out of the postings when it goes,
and its field lengths, for normalising.
-}
type alias Words =
    { terms : List String
    , lengths : Fields Float
    }


repoOf : Entry -> Repo
repoOf entry =
    case entry of
        Queued repo ->
            repo

        Indexed repo _ ->
            repo


{-| The repos a term appears in, with how often it appears in each of their fields.
-}
type alias Posting =
    { count : Int
    , docs : Dict Int (Fields Float)
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
        , count = 0
        , queue = Set.empty
        , postings = Dict.empty
        , totals = zeroes
        , indexed = 0
        , vocabulary = { terms = Dict.empty, size = 0 }
        , avatars = Dict.empty
        , facets = { languages = Dict.empty, topics = Dict.empty, owners = Dict.empty }
        }


zeroes : Fields Float
zeroes =
    { name = 0, owner = 0, topics = 0, description = 0, language = 0 }


fromList : List Repo -> Index
fromList repos =
    upsert repos empty


{-| Adds repos, or replaces them, queued for indexing.
-}
upsert : List Repo -> Index -> Index
upsert repos index =
    List.foldl
        (\repo acc ->
            let
                (Index data) =
                    removeOne repo.id acc
            in
            Index
                { data
                    | entries = Dict.insert repo.id (Queued repo) data.entries
                    , count = data.count + 1
                    , queue = Set.insert repo.id data.queue
                    , avatars = Dict.insert repo.owner repo.avatar data.avatars
                    , facets = countFacets 1 repo data.facets
                }
        )
        index
        repos


remove : List Int -> Index -> Index
remove repoIds index =
    List.foldl removeOne index repoIds


removeOne : Int -> Index -> Index
removeOne id (Index data) =
    case Dict.get id data.entries of
        Just (Indexed repo words) ->
            Index
                { data
                    | entries = Dict.remove id data.entries
                    , count = data.count - 1
                    , postings = List.foldl (removePosting id) data.postings words.terms
                    , totals = subtractFields words.lengths data.totals
                    , indexed = data.indexed - 1
                    , facets = countFacets -1 repo data.facets
                }

        Just (Queued repo) ->
            Index
                { data
                    | entries = Dict.remove id data.entries
                    , count = data.count - 1
                    , queue = Set.remove id data.queue
                    , facets = countFacets -1 repo data.facets
                }

        Nothing ->
            Index data


{-| Counts a repo's language, topics and owner in or, with -1, out.
-}
countFacets : Int -> Repo -> Facets -> Facets
countFacets delta repo counts =
    let
        bump value =
            Dict.update value
                (\current ->
                    let
                        next =
                            Maybe.withDefault 0 current + delta
                    in
                    if next > 0 then
                        Just next

                    else
                        Nothing
                )
    in
    { languages = List.foldl bump counts.languages (Filters.valuesOf Filters.Language repo)
    , topics = List.foldl bump counts.topics (Filters.valuesOf Filters.Topic repo)
    , owners = List.foldl bump counts.owners (Filters.valuesOf Filters.Owner repo)
    }


{-| Whether some repos still wait to be indexed for search.
-}
pending : Index -> Bool
pending (Index data) =
    not (Set.isEmpty data.queue)


{-| Indexes up to `batch` of the waiting repos.
-}
settle : Int -> Index -> Index
settle batch ((Index data) as index) =
    if Set.isEmpty data.queue then
        index

    else
        let
            ( _, next ) =
                Set.foldl
                    (\id ( taken, acc ) ->
                        if taken < batch then
                            ( taken + 1, id :: acc )

                        else
                            ( taken, acc )
                    )
                    ( 0, [] )
                    data.queue

            (Index done) =
                List.foldl indexOne (Index { data | queue = List.foldl Set.remove data.queue next }) next
        in
        if Set.isEmpty done.queue then
            withVocabulary (Index done)

        else
            Index done


settleAll : Index -> Index
settleAll ((Index data) as index) =
    settle (Set.size data.queue) index


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


{-| Puts a queued repo's terms in the postings.
-}
indexOne : Int -> Index -> Index
indexOne id ((Index data) as index) =
    case Dict.get id data.entries of
        Just (Queued repo) ->
            insert repo index

        _ ->
            index


insert : Repo -> Index -> Index
insert repo (Index data) =
    let
        fields =
            tokenize repo

        lengths =
            lengthsOf fields

        frequencies =
            Dict.empty
                |> countField (\f -> { f | name = f.name + 1 }) fields.name
                |> countField (\f -> { f | owner = f.owner + 1 }) fields.owner
                |> countField (\f -> { f | topics = f.topics + 1 }) fields.topics
                |> countField (\f -> { f | description = f.description + 1 }) fields.description
                |> countField (\f -> { f | language = f.language + 1 }) fields.language

        postings =
            Dict.foldl
                (\term counts acc ->
                    Dict.update term
                        (\existing ->
                            case existing of
                                Just posting ->
                                    Just { count = posting.count + 1, docs = Dict.insert repo.id counts posting.docs }

                                Nothing ->
                                    Just { count = 1, docs = Dict.singleton repo.id counts }
                        )
                        acc
                )
                data.postings
                frequencies
    in
    Index
        { data
            | entries =
                Dict.insert repo.id (Indexed repo { terms = Dict.keys frequencies, lengths = lengths }) data.entries
            , postings = postings
            , totals = addFields lengths data.totals
            , indexed = data.indexed + 1
        }


{-| Counts each token of a field, with `bump` adding one to that field.
-}
countField : (Fields Float -> Fields Float) -> List String -> Dict String (Fields Float) -> Dict String (Fields Float)
countField bump tokens frequencies =
    List.foldl
        (\token acc -> Dict.update token (\current -> Just (bump (Maybe.withDefault zeroes current))) acc)
        frequencies
        tokens


{-| A term's frequency in a repo: its count in each field, normalised by how long that field
is against the library's average, and weighted by how much the field says.
-}
frequency : Fields Float -> Fields Float -> Fields Float -> Float
frequency average lengths counts =
    let
        field { weight, b } mean length count =
            if count == 0 then
                0

            else
                count * weight / (1 - b + b * length / max 1 mean)
    in
    field weighting.name average.name lengths.name counts.name
        + field weighting.owner average.owner lengths.owner counts.owner
        + field weighting.topics average.topics lengths.topics counts.topics
        + field weighting.description average.description lengths.description counts.description
        + field weighting.language average.language lengths.language counts.language


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
    Dict.get id data.entries |> Maybe.map repoOf


ids : Index -> List Int
ids (Index data) =
    Dict.keys data.entries


{-| How many repos have each language, topic and owner, over the whole library.
-}
facets : Index -> Facets
facets (Index data) =
    data.facets


{-| The avatar of an owner of any starred repo.
-}
avatar : String -> Index -> Maybe String
avatar owner (Index data) =
    Dict.get owner data.avatars


{-| The fewest and the most stars of any repo in the index.
-}
starBounds : Index -> Maybe ( Int, Int )
starBounds (Index data) =
    Dict.foldl
        (\_ entry bounds ->
            let
                stars =
                    (repoOf entry).stars
            in
            case bounds of
                Just ( low, high ) ->
                    Just ( min low stars, max high stars )

                Nothing ->
                    Just ( stars, stars )
        )
        Nothing
        data.entries


toList : Index -> List Repo
toList (Index data) =
    Dict.foldr (\_ entry acc -> repoOf entry :: acc) [] data.entries



-- SEARCH


{-| The relevance of every repo that matches all the words of the query, or `Nothing` when
the query has no words, and so doesn't narrow anything.
-}
score : String -> Index -> Maybe (Dict Int Float)
score query index =
    case unique (Tokens.words query) of
        [] ->
            Nothing

        first :: rest ->
            let
                (Index data) =
                    settleAll index

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
                repo =
                    repoOf entry

                name =
                    String.filter Char.isAlphaNum (String.toLower repo.name)

                factor =
                    if name == phrase then
                        3

                    else if String.startsWith phrase name then
                        1.5

                    else
                        1
            in
            factor * relevance + 0.05 * logBase 10 (1 + toFloat repo.stars)

        Nothing ->
            relevance


{-| Scores one query word against a settled index, where every repo is indexed.
-}
scoreWord : { a | postings : Dict String Posting, vocabulary : Vocabulary, entries : Dict Int Entry, totals : Fields Float, indexed : Int } -> String -> Dict Int Float
scoreWord data word =
    let
        n =
            toFloat data.indexed

        average =
            averages data.totals data.indexed
    in
    expand word data
        |> List.foldl
            (\( factor, posting ) acc ->
                let
                    df =
                        toFloat posting.count

                    idf =
                        logBase e (1 + (n - df + 0.5) / (df + 0.5))
                in
                Dict.foldl
                    (\id counts inner ->
                        case Dict.get id data.entries of
                            Just (Indexed _ words) ->
                                let
                                    tf =
                                        frequency average words.lengths counts

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

                            _ ->
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
