module Logo exposing (view)

{-| The STRS wordmark: single strokes, the R's bowl landing on its diagonal. The same
drawing as `public/logo.svg`, with a stroke that stays the same width at any size.
-}

import Html exposing (Html)
import VirtualDom
import Svg exposing (path, svg)
import Svg.Attributes exposing (class, d, fill, stroke, strokeLinejoin, strokeWidth, transform, viewBox)


view : String -> Html msg
view classes =
    svg
        [ viewBox "40 52 690 186"
        , fill "none"
        , stroke "currentColor"
        , strokeWidth "1.75"
        , strokeLinejoin "miter"
        , class classes
        ]
        (List.map
            (\attrs -> path (VirtualDom.attribute "vector-effect" "non-scaling-stroke" :: attrs) [])
            [ [ d s ]
            , [ d "M225 66H360M292 66V232" ]
            , [ d "M417 62V228H530L420 66M417 66H476A40 40 0 0 1 476 146" ]
            , [ d s, transform "translate(535 0)" ]
            ]
        )


s : String
s =
    "M178 108C178 80 150 62 115 62 80 62 54 80 54 104 54 128 75 138 115 145 160 152 180 165 180 190 180 215 152 228 115 228 78 228 52 212 50 178"
