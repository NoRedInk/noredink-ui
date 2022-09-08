module Code exposing
    ( string, maybeString
    , maybe
    , maybeFloat
    , bool
    , commentInline
    , list, listMultiline
    , record, recordMultiline
    , pipelineMultiline
    , newlineWithIndent
    , withParens
    , always
    )

{-|

@docs string, maybeString
@docs maybe
@docs maybeFloat
@docs bool
@docs commentInline
@docs list, listMultiline
@docs record, recordMultiline
@docs pipelineMultiline
@docs newlineWithIndent
@docs withParens
@docs always

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
    indents ++ "[ " ++ String.join (indents ++ ", ") list_ ++ indents ++ "]"


{-| -}
record : List ( String, String ) -> String
record items =
    let
        monolineRecord =
            recordSingleline items
    in
    if String.length monolineRecord > 80 then
        recordMultiline items 1

    else
        monolineRecord


{-| -}
recordSingleline : List ( String, String ) -> String
recordSingleline items =
    "{ "
        ++ String.join ", "
            (List.map (\( key, value ) -> key ++ " = " ++ value) items)
        ++ "} "


{-| -}
recordMultiline : List ( String, String ) -> Int -> String
recordMultiline items indent =
    let
        indents =
            newlineWithIndent indent
    in
    indents
        ++ "{ "
        ++ String.join (indents ++ ", ")
            (List.map (\( key, value ) -> key ++ " = " ++ value) items)
        ++ indents
        ++ "}"


{-| -}
pipelineMultiline : List String -> Int -> String
pipelineMultiline pipedWith indent =
    let
        indents =
            newlineWithIndent (indent + 1)
    in
    newlineWithIndent indent ++ String.join (indents ++ "|> ") pipedWith


newlineWithIndent : Int -> String
newlineWithIndent indent =
    "\n" ++ String.repeat indent "    "


{-| -}
withParens : String -> String
withParens val =
    "(" ++ val ++ ")"


{-| -}
always : String -> String
always val =
    "\\_ -> " ++ val
