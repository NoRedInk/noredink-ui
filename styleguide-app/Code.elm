module Code exposing
    ( f, fExposed, val, hardcode, always
    , string, maybeString
    , maybeFloat
    , bool
    , commentInline
    , list, listMultiline
    )

{-|

@docs f, fExposed, val, hardcode, always
@docs string, maybeString
@docs maybeFloat
@docs bool
@docs commentInline
@docs list, listMultiline

-}

import Elm exposing (Expression)


f : String -> String -> List Expression -> Expression
f moduleName name =
    Elm.apply
        (Elm.value
            { importFrom = [ moduleName ]
            , name = name
            , annotation = Nothing
            }
        )


fExposed : String -> List Expression -> Expression
fExposed name =
    Elm.apply (hardcode name)


val : String -> String -> Expression
val moduleName name =
    Elm.value
        { importFrom = [ moduleName ]
        , name = name
        , annotation = Nothing
        }


hardcode : String -> Expression
hardcode value =
    Elm.value { importFrom = [], name = value, annotation = Nothing }


always : Expression -> Expression
always expression =
    Elm.fn ( "_", Nothing ) (\_ -> expression)


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
