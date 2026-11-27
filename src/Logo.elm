module Logo exposing (view)

{-| The wordmark, set in Major Mono Display. Its lower case are thin capitals and its upper
case R is the one with the triangle, so "stRs" reads as STRS with that R.
-}

import Html exposing (Html, span, text)
import Html.Attributes exposing (attribute, class)


view : String -> Html msg
view classes =
    span [ class ("font-logo leading-none whitespace-nowrap select-none " ++ classes), attribute "aria-hidden" "true" ]
        [ text "stRs" ]
