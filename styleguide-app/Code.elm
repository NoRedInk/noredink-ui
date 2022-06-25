module Code exposing
    ( string, maybeString
    , maybeFloat
    , maybe
    )

{-|

@docs string, maybeString
@docs maybeFloat
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


{-| -}
maybeFloat : Maybe Float -> String
maybeFloat =
    maybe << Maybe.map String.fromFloat
