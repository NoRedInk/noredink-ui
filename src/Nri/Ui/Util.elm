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
        unsafeChar =
            regexFromString "(^[^a-zA-Z]+|[^a-zA-Z0-9_-]+)"

        collapse =
            regexFromString "[_-\\s]+"
    in
    Regex.replace unsafeChar
        (\{ index } ->
            if index == 0 then
                ""

            else
                "-"
        )
        >> Regex.replace collapse (always "-")
        >> String.toLower


regexFromString : String -> Regex
regexFromString =
    Regex.fromString >> Maybe.withDefault Regex.never
