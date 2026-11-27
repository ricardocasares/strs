module Language exposing (color)

import Dict exposing (Dict)


{-| GitHub's linguist colour for the most common languages, and a neutral grey for the rest.
-}
color : String -> String
color language =
    Dict.get language colors |> Maybe.withDefault "#8b949e"


colors : Dict String String
colors =
    Dict.fromList
        [ ( "Astro", "#ff5a03" )
        , ( "C", "#555555" )
        , ( "C#", "#178600" )
        , ( "C++", "#f34b7d" )
        , ( "Clojure", "#db5855" )
        , ( "CSS", "#663399" )
        , ( "Crystal", "#000100" )
        , ( "Dart", "#00B4AB" )
        , ( "Dockerfile", "#384d54" )
        , ( "Elixir", "#6e4a7e" )
        , ( "Elm", "#60B5CC" )
        , ( "Emacs Lisp", "#c065db" )
        , ( "Erlang", "#B83998" )
        , ( "F#", "#b845fc" )
        , ( "Gleam", "#ffaff3" )
        , ( "Go", "#00ADD8" )
        , ( "GLSL", "#5686a5" )
        , ( "Groovy", "#4298b8" )
        , ( "Haskell", "#5e5086" )
        , ( "HCL", "#844FBA" )
        , ( "HTML", "#e34c26" )
        , ( "Java", "#b07219" )
        , ( "JavaScript", "#f1e05a" )
        , ( "Julia", "#a270ba" )
        , ( "Jupyter Notebook", "#DA5B0B" )
        , ( "Kotlin", "#A97BFF" )
        , ( "Lean", "#5c6bc0" )
        , ( "Lua", "#000080" )
        , ( "Makefile", "#427819" )
        , ( "MDX", "#fcb32c" )
        , ( "Nim", "#ffc200" )
        , ( "Nix", "#7e7eff" )
        , ( "Objective-C", "#438eff" )
        , ( "OCaml", "#ef7a08" )
        , ( "Odin", "#60AFFE" )
        , ( "Perl", "#0298c3" )
        , ( "PHP", "#4F5D95" )
        , ( "PowerShell", "#012456" )
        , ( "PureScript", "#1D222D" )
        , ( "Python", "#3572A5" )
        , ( "R", "#198CE7" )
        , ( "Racket", "#3c5caa" )
        , ( "ReScript", "#ed5051" )
        , ( "Ruby", "#701516" )
        , ( "Rust", "#dea584" )
        , ( "Scala", "#c22d40" )
        , ( "SCSS", "#c6538c" )
        , ( "Shell", "#89e051" )
        , ( "Solidity", "#AA6746" )
        , ( "Svelte", "#ff3e00" )
        , ( "Swift", "#F05138" )
        , ( "TeX", "#3D6117" )
        , ( "TypeScript", "#3178c6" )
        , ( "Vim Script", "#199f4b" )
        , ( "Vue", "#41b883" )
        , ( "WebAssembly", "#04133b" )
        , ( "Zig", "#ec915c" )
        ]
