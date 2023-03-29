module Nri.Ui.Util exposing (dashify, isSubstringWithinDistance, levenshteinDistance, removePunctuation, safeIdString)

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
(removes everything that isn't whitespace, alphanumeric, or an underscore).
-}
removePunctuation : String -> String
removePunctuation =
    let
        regex =
            Regex.fromString "[^A-Za-z0-9\\w\\s]"
                |> Maybe.withDefault Regex.never
    in
    Regex.replace regex (always "")


{-| Creates a lowercased string that is safe to use for HTML IDs.
Ensures that nonletter characters are cut from the front and replaces bad characters with a dash
-}
safeIdString : String -> String
safeIdString =
    let
        nonAlphaAtStart =
            -- From the start of the string, match the contiguous block of
            -- not ASCII letters at the start of the String.
            -- Why [a-zA-Z] and not [A-z]?
            -- There are punctuation characters in that range: [\]^_` all come after Z and before a
            "^[^a-zA-Z]+"

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

        anyOfThese strs =
            "(" ++ String.join "|" strs ++ ")"

        unsafeChar =
            [ nonAlphaAtStart
            , nonAlphaNumUnderscoreHyphenAnywhere
            ]
                |> anyOfThese
                |> regexFromString

        nonAlphaNumAtEnd =
            -- any contiguous block of letters that aren't any of
            -- + ASCII letters
            -- + numbers
            regexFromString "[^a-zA-Z0-9]+$"

        collapsePunctuationToOne =
            regexFromString "[_-]+"
    in
    Regex.replace nonAlphaNumAtEnd (always "")
        >> Regex.replace unsafeChar
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


levenshteinDistance : String -> String -> Int
levenshteinDistance s t =
    levenshteinDistanceHelper 0 (String.toList s) (String.toList t)


levenshteinDistanceHelper : Int -> List Char -> List Char -> Int
levenshteinDistanceHelper distance s t =
    case ( s, t ) of
        ( [], _ ) ->
            distance + List.length t

        ( _, [] ) ->
            distance + List.length s

        ( sHead :: sTail, tHead :: tTail ) ->
            let
                cost =
                    if sHead == tHead then
                        0

                    else
                        1
            in
            levenshteinDistanceHelper (distance + cost) sTail tTail


isSubstringWithinDistance : String -> String -> Int -> Bool
isSubstringWithinDistance s t d =
    let
        sLength =
            String.length s

        tLength =
            String.length t

        sList =
            String.toList s

        tList =
            String.toList t
    in
    if sLength > tLength then
        False

    else
        List.any
            (\i ->
                levenshteinDistanceHelper
                    0
                    (List.take sLength (List.drop i tList))
                    sList
                    <= d
            )
            (List.range 0 (tLength - sLength))
