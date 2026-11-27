module Logo exposing (view)

{-| The STRS wordmark: single strokes, the R's bowl meeting its diagonal halfway down. The
same drawing as `public/logo.svg`, with the stroke in proportion to the letters.
-}

import Html exposing (Html)
import Svg exposing (path, svg)
import Svg.Attributes exposing (class, d, fill, stroke, strokeLinejoin, strokeMiterlimit, strokeWidth, transform, viewBox)


view : String -> Html msg
view classes =
    svg
        [ viewBox "84 36 676 178"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "7"
        , strokeLinejoin "miter"
        , strokeMiterlimit "10"
        , class classes
        ]
        [ path [ d s ] []
        , path [ d "M265 46H400M332 46V212" ] []
        , path [ d "M520 130H534A42 42 0 0 0 534 46H455V207H576L459 46" ] []
        , path [ d s, transform "translate(535 0)" ] []
        ]


s : String
s =
    "M219 94C219 61 192 42 157 42 122 42 96 60 96 84 96 108 117 118 157 125 202 132 222 145 222 170 222 195 194 208 157 208 120 208 93 191 92 155"
