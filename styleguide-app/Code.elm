module Code exposing
    ( string, maybeString
    , maybeFloat
    , bool
    , commentInline
    , list, listMultiline
    )

{-|

@docs string, maybeString
@docs maybeFloat
@docs bool
@docs commentInline
@docs list, listMultiline

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


{-| -}
commentInline : String -> String
commentInline comment =
    "-- " ++ comment


{-| -}
list : List String -> String
list list_ =
    let
        monolineList =
            listSingleline list_
    in
    if String.length monolineList > 80 then
        listMultiline list_ 1

    else
        monolineList


{-| -}
listSingleline : List String -> String
listSingleline list_ =
    "[ " ++ String.join ", " list_ ++ " ]"


{-| -}
listMultiline : List String -> Int -> String
listMultiline list_ indent =
    let
        indents =
            newlineWithIndent indent
    in
    indents ++ "[ " ++ String.join (indents ++ ", ") list_ ++ indents ++ "] "


newlineWithIndent : Int -> String
newlineWithIndent indent =
    String.repeat indent "\n    "
