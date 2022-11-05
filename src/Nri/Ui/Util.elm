module Nri.Ui.Util exposing (dashify, safeId, safeIdWithPrefix)

import Regex exposing (Regex)


{-| Convenience method for going from a string with spaces to a string with dashes.
- possibly worth removing since the safeId methods do this and more.
-}
dashify : String -> String
dashify =
    String.trim
        >> Regex.replace
            parts.regexes.whiteSpace
            parts.replacements.hyphenMinus


type alias RegexHelpers a =
    { nonAlphaAtStart : a
    , nonAlphaNumAtEnd : a
    , nonAlphaNumUnderscoreHyphenAnywhere : a
    , contiguousIdSafePunctuation : a
    , whiteSpace : a
    , anyOfThese : List String -> a
    }


parts :
    { regexes : RegexHelpers Regex
    , strings : RegexHelpers String
    , replacements :
        { nothing : a -> String
        , hyphenMinus : a -> String
        }
    }
parts =
    let
        regexFromString : String -> Regex
        regexFromString =
            Regex.fromString >> Maybe.withDefault Regex.never

        strings : RegexHelpers String
        strings =
            { nonAlphaAtStart =
                -- From the start of the string, match the contiguous block of
                -- not ASCII letters at the start of the String.
                -- Why [a-zA-Z] and not [A-z]?
                -- There are punctuation characters in that range: [\]^_` all come after Z and before a
                -- Why? So that miniscule letters would be the same bit pattern as their majuscule equivalents,
                -- except with the 32 place bit set to 1 instead of 0: A is 65, a is 97.
                "^[^a-zA-Z]+"
            , nonAlphaNumAtEnd =
                -- The contiguous block of letters that aren't any of
                -- + ASCII letters
                -- + numbers
                -- followed immediately by the end of the string
                "[^a-zA-Z0-9]+$"
            , nonAlphaNumUnderscoreHyphenAnywhere =
                -- any contiguous block of characters that aren't any of
                -- + ASCII letters
                -- + numbers
                -- + underscore
                -- + the hyphen-minus character commonly called "dash" (seen in between "hyphen" and "minus" on this line)
                -- This does not need the + at the end; Regex.replace is global by default
                -- but we pay a penalty for calling the replacement function, so
                -- calling it once per contiguous group is an easy way to cut down on that.
                "[^a-zA-Z0-9_-]+"
            , contiguousIdSafePunctuation =
                -- any contiguous block of
                -- + underscores or
                -- + hyphen-minus characters
                "[_-]+"
            , whiteSpace =
                -- any contiguous block of whitespace characters
                "\\s+"
            , anyOfThese = \strs -> "(" ++ String.join "|" strs ++ ")"
            }
    in
    { strings = strings
    , regexes =
        { nonAlphaAtStart = regexFromString strings.nonAlphaAtStart
        , nonAlphaNumAtEnd = regexFromString strings.nonAlphaNumAtEnd
        , nonAlphaNumUnderscoreHyphenAnywhere = regexFromString strings.nonAlphaNumUnderscoreHyphenAnywhere
        , contiguousIdSafePunctuation = regexFromString strings.contiguousIdSafePunctuation
        , whiteSpace = regexFromString strings.whiteSpace
        , anyOfThese = strings.anyOfThese >> regexFromString
        }
    , replacements =
        { nothing = always ""
        , hyphenMinus = always "-"
        }
    }


{-| Turn any string into a lowercased safe ID value.
Trust whoever invokes this to make the prefix a safe ID string,
meaning starting with an ASCII letter, not a number or punctuation
-}
safeIdWithPrefix : String -> String -> String
safeIdWithPrefix prefix string =
    prefix
        ++ "-"
        ++ string
        |> Regex.replace
            parts.regexes.nonAlphaNumAtEnd
            parts.replacements.nothing
        |> Regex.replace
            parts.regexes.nonAlphaNumUnderscoreHyphenAnywhere
            parts.replacements.hyphenMinus
        |> Regex.replace
            parts.regexes.contiguousIdSafePunctuation
            parts.replacements.hyphenMinus
        |> String.toLower


{-| Creates a lowercased string that is safe to use for HTML IDs.
Ensures that nonletter characters are cut from the front and replaces bad characters with a dash
-}
safeId : String -> String
safeId =
    Regex.replace
        (parts.regexes.anyOfThese
            [ -- Why no numbers at the start of the string? Invalid IDs for the most part.
              -- You cannot use '#1st-element' in CSS, querySelector, or querySelectorAll.
              -- They don't just not find the element, they actually throw a
              --   [SyntaxError](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SyntaxError)
              -- You CAN use it in getElementById but why not enforce something that works everywhere?
              -- Here's a codepen demonstrating it https://codepen.io/ap-nri/pen/OJENQLY
              parts.strings.nonAlphaAtStart
            , parts.strings.nonAlphaNumAtEnd
            ]
        )
        parts.replacements.nothing
        >> Regex.replace
            parts.regexes.nonAlphaNumUnderscoreHyphenAnywhere
            parts.replacements.hyphenMinus
        >> Regex.replace
            parts.regexes.contiguousIdSafePunctuation
            parts.replacements.hyphenMinus
        >> String.toLower
