module Nri.Ui.Util exposing (dashify, removePunctuation, safeIdString)

import Regex


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
-}
safeIdString : String -> String
safeIdString =
    let
        unsafeChar =
            Regex.fromString "(^[^a-zA-Z]+|[^a-zA-Z0-9_-]+)" |> Maybe.withDefault Regex.never
    in
    Regex.replace unsafeChar
        (\{ index } ->
            if index == 0 then
                ""

            else
                "-"
        )
        >> String.toLower
