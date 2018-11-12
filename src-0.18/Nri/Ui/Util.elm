module Nri.Ui.Util exposing (dashify, removePunctuation)

import Regex


{-| Convenience method for going from a string with spaces to a string with dashes.
-}
dashify : String -> String
dashify =
    Regex.replace Regex.All (Regex.regex " ") (always "-")


{-| Convenience method for removing punctuation
(removes everything that isn't whitespace or alphanumeric).
-}
removePunctuation : String -> String
removePunctuation =
    Regex.replace Regex.All (Regex.regex "[^A-z0-9\\w\\s]") (always "")
