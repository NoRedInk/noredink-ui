module Code exposing
    ( string, maybeString
    , maybe
    )

{-|

@docs string, maybeString
@docs maybe

-}


{-| -}
string : String -> String
string s =
    "\"" ++ s ++ "\""


{-| -}
maybe : Maybe String -> String
maybe =
    Maybe.map (\s -> "Just " ++ s) >> Maybe.withDefault "Nothing"


{-| -}
maybeString : Maybe String -> String
maybeString =
    maybe << Maybe.map string
