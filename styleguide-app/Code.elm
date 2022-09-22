module Code exposing
    ( string, maybeString
    , maybe
    , maybeFloat
    , bool
    , commentInline
    , list, listMultiline
    , tuple
    , record, recordMultiline
    , newlineWithIndent, newlines
    , withParens
    , anonymousFunction, always
    , caseExpression
    , browserElement, unstyledView
    , fromModule
    , var
    )

{-|

@docs string, maybeString
@docs maybe
@docs maybeFloat
@docs bool
@docs commentInline
@docs list, listMultiline
@docs tuple
@docs record, recordMultiline
@docs newlineWithIndent, newlines
@docs withParens
@docs anonymousFunction, always
@docs caseExpression
@docs browserElement, unstyledView
@docs always
@docs fromModule
@docs var

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


structure : String -> String -> List String -> String
structure open close items =
    let
        monolineStructure =
            structureSingleline open close items
    in
    if String.length monolineStructure > 80 then
        structureMultiline open close items 1

    else
        monolineStructure


structureSingleline : String -> String -> List String -> String
structureSingleline open close items =
    open ++ " " ++ String.join ", " items ++ " " ++ close


structureMultiline : String -> String -> List String -> Int -> String
structureMultiline open close items indent =
    let
        indents =
            newlineWithIndent indent
    in
    (indents ++ open ++ " ")
        ++ String.join (indents ++ ", ") items
        ++ (indents ++ "" ++ close)


{-| -}
tuple : String -> String -> String
tuple a b =
    structure "(" ")" [ a, b ]


{-| -}
list : List String -> String
list =
    structure "[" "]"


{-| -}
listMultiline : List String -> Int -> String
listMultiline =
    structureMultiline "[" "]"


recordValues : List ( String, String ) -> List String
recordValues =
    List.map (\( key, value ) -> key ++ " = " ++ value)


{-| -}
record : List ( String, String ) -> String
record =
    structure "{" "}" << recordValues


{-| -}
recordMultiline : List ( String, String ) -> Int -> String
recordMultiline items =
    structureMultiline "{" "}" (recordValues items)


{-| -}
pipelineMultiline : List String -> Int -> String
pipelineMultiline pipedWith indent =
    let
        indents =
            newlineWithIndent (indent + 1)
    in
    newlineWithIndent indent ++ String.join (indents ++ "|> ") pipedWith


newlines : String
newlines =
    newlineWithIndent 0 ++ newlineWithIndent 0


newlineWithIndent : Int -> String
newlineWithIndent indent =
    "\n" ++ String.repeat indent "    "


{-| -}
withParens : String -> String
withParens val =
    "(" ++ val ++ ")"


{-| -}
anonymousFunction : String -> String -> String
anonymousFunction vars body =
    "\\" ++ vars ++ " -> " ++ body


{-| -}
always : String -> String
always val =
    anonymousFunction "_" val


{-| -}
caseExpression : String -> List ( String, String ) -> Int -> String
caseExpression var_ results indent =
    let
        matchIndents =
            newlineWithIndent (indent + 1)

        resultIndents =
            newlineWithIndent (indent + 2)

        toCase ( match, result ) =
            match ++ " -> " ++ resultIndents ++ result
    in
    ((newlineWithIndent indent ++ "case " ++ var_ ++ " of") :: List.map toCase results)
        |> String.join matchIndents


{-| -}
browserElement : { init : String, view : String, update : String, subscriptions : String } -> String
browserElement { init, view, update, subscriptions } =
    String.join (newlineWithIndent 1)
        [ "Browser.element"
        , recordMultiline
            [ ( "init", init )
            , ( "view", view )
            , ( "update", update )
            , ( "subscriptions", subscriptions )
            ]
            2
        ]


{-| -}
unstyledView : String -> String
unstyledView view =
    pipelineMultiline [ view, "toUnstyled" ] 1


{-| -}
fromModule : String -> String -> String
fromModule moduleName name =
    moduleName ++ "." ++ name


{-| -}
var : String -> Int -> String -> String
var varName indent body =
    varName ++ " =" ++ newlineWithIndent indent ++ body
