module Nri.Ui.Util exposing (safeId, safeIdWithPrefix)

import Regex exposing (Regex)


hyphenMinus : any -> String
hyphenMinus _ =
    "-"


nonAlphaNumUnderscoreHyphenAnywhere : Regex
nonAlphaNumUnderscoreHyphenAnywhere =
    -- any contiguous block of characters that aren't any of
    -- + ASCII letters
    -- + numbers
    -- + underscore
    -- + the hyphen-minus character commonly called "dash" (seen in between "hyphen" and "minus" on this line)
    -- This does not need the + at the end; Regex.replace is global by default
    -- but we pay a penalty for calling the replacement function, so
    -- calling it once per contiguous group is an easy way to cut down on that.
    "[^a-zA-Z0-9_-]+"
        |> Regex.fromString
        |> Maybe.withDefault Regex.never


{-| Prepends a prefix to the result of safeId.
-}
safeIdWithPrefix : String -> String -> String
safeIdWithPrefix prefix string =
    safeId
        (prefix
            ++ "-"
            ++ string
        )


{-| Creates a lowercased string that is safe to use for HTML IDs.
Removes all groups of unsafe characters and replaces each group with a dash.
prepends "id-" to the result to ensure that the ID starts with a letter. (necessary for CSS selectors including getElementById)
-}
safeId : String -> String
safeId unsafe =
    "id-"
        ++ Regex.replace
            nonAlphaNumUnderscoreHyphenAnywhere
            hyphenMinus
            unsafe
