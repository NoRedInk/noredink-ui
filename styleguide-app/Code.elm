module Code exposing
    ( string, maybeString
    , maybeFloat
    , bool
    )

{-|

@docs string, maybeString
@docs maybeFloat
@docs bool

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


{-| -}
bool : Bool -> String
bool =
    Debug.toString
