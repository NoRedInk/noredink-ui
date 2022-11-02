module Nri.Ui.Util exposing (dashify, removePunctuation, safeIdString)

import Regex exposing (Regex)


{-| Convenience method for going from a string with spaces to a string with dashes.
-}
dashify : String -> String
dashify =
    let
        regex =
            Regex.fromString " "
                |> Maybe.withDefault Regex.never
    in
    Regex.replace regex (always "-")


{-| Convenience method for removing punctuation
(removes everything that isn't whitespace or alphanumeric).
-}
removePunctuation : String -> String
removePunctuation =
    let
        regex =
            Regex.fromString "[^A-z0-9\\w\\s]"
                |> Maybe.withDefault Regex.never
    in
    Regex.replace regex (always "")


{-| Creates a lowercased string that is safe to use for HTML IDs.
Ensures that nonletter characters are cut from the front and replaces bad characters with a dash
-}
safeIdString : String -> String
safeIdString =
    let
        nonAlphaAtStartOfString =
            -- From the start of the string, match the contiguous block of
            -- not ASCII letters at the start of the String.
            -- Note majiscule letters are before miniscule in the range,
            -- so [A-z] is the same as [A-Za-z] as above in removePunctuation
            "^[^A-z]+"

        nonAlphaNumUnderscoreHyphenAnywhere =
            -- match any contiguous block of letters that aren't any of
            -- + ASCII letters
            -- + numbers
            -- + underscore
            -- + the hyphen-minus character commonly called "dash" (seen in between "hyphen" and "minus" on this line)
            -- This does not need the + at the end; Regex.replace is global by default
            -- but we pay a penalty for calling the replacement function, so
            -- calling it once per contiguous group is an easy way to cut down on that.
            "[^A-z0-9_-]+"

        anyOfThese strs =
            "(" ++ String.join "|" strs ++ ")"

        unsafeChar =
            [ nonAlphaAtStartOfString
            , nonAlphaNumUnderscoreHyphenAnywhere
            ]
                |> anyOfThese
                |> regexFromString

        collapsePunctuationToOne =
            -- match any contiguous group of either underscore or hyphen-minus characters
            regexFromString "[_-]+"
    in
    Regex.replace unsafeChar
        (\{ index } ->
            if index == 0 then
                ""

            else
                "-"
        )
        >> Regex.replace collapsePunctuationToOne (always "-")
        >> String.toLower


regexFromString : String -> Regex
regexFromString =
    Regex.fromString >> Maybe.withDefault Regex.never
