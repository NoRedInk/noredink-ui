module Code exposing
    ( string, stringMultiline, maybeString
    , maybe
    , bool
    , commentInline
    , list, listMultiline, listMultilineFlat
    , pipelineMultiline
    , record, recordMultiline
    , listOfRecordsMultiline
    , newlineWithIndent, newline, newlines
    , withParens, withParensMultiline
    , anonymousFunction, always
    , caseExpression
    , browserElement, unstyledView, unstyledViewWithIndent
    , fromModule
    , var, varWithTypeAnnotation
    , funcWithType
    , unionType
    , apply
    , int, tuple, tupleMultiline
    )

{-|

@docs string, stringMultiline, maybeString
@docs maybe
@docs bool
@docs commentInline
@docs list, listMultiline, listMultilineFlat
@docs tuple tupleMultiline
@docs pipelineMultiline
@docs record, recordMultiline
@docs listOfRecordsMultiline
@docs newlineWithIndent, newline, newlines
@docs withParens, withParensMultiline
@docs anonymousFunction, always
@docs caseExpression
@docs browserElement, unstyledView, unstyledViewWithIndent
@docs always
@docs fromModule
@docs var, varWithTypeAnnotation
@docs funcWithType
@docs unionType
@docs apply

-}


{-| -}
string : String -> String
string s =
    "\"" ++ s ++ "\""


{-| -}
stringMultiline : String -> String
stringMultiline s =
    newlineWithIndent 1
        ++ "\"\"\""
        ++ String.replace "\n" (newlineWithIndent 0) s
        ++ newlineWithIndent 0
        ++ "\"\"\""


{-| -}
maybe : Maybe String -> String
maybe =
    Maybe.map (\s -> "Just " ++ s) >> Maybe.withDefault "Nothing"


{-| -}
maybeString : Maybe String -> String
maybeString =
    maybe << Maybe.map string


{-| -}
bool : Bool -> String
bool =
    Debug.toString


{-| -}
int : Int -> String
int =
    String.fromInt


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
    if List.length items == 0 then
        open ++ close

    else if String.length monolineStructure > 80 then
        structureMultiline open close items 1

    else
        monolineStructure


structureSingleline : String -> String -> List String -> String
structureSingleline open close items =
    open ++ " " ++ String.join ", " items ++ " " ++ close


structureMultiline : String -> String -> List String -> Int -> String
structureMultiline open close items indent =
    newlineWithIndent indent ++ structureMultilineFlat open close items indent


structureMultilineFlat : String -> String -> List String -> Int -> String
structureMultilineFlat open close items indent =
    let
        indents =
            newlineWithIndent indent
    in
    if List.length items == 0 then
        open ++ close

    else
        (open ++ " ")
            ++ String.join (indents ++ ", ") items
            ++ (indents ++ close)


{-| -}
tuple : String -> String -> String
tuple a b =
    structure "(" ")" [ a, b ]


{-| -}
tupleMultiline : String -> String -> Int -> String
tupleMultiline a b =
    structureMultiline "(" ")" [ a, b ]


{-| -}
list : List String -> String
list =
    structure "[" "]"


{-| -}
listMultiline : List String -> Int -> String
listMultiline =
    structureMultiline "[" "]"


{-| -}
listMultilineFlat : List String -> Int -> String
listMultilineFlat =
    structureMultilineFlat "[" "]"


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
recordMultilineFlat : List ( String, String ) -> Int -> String
recordMultilineFlat items =
    structureMultilineFlat "{" "}" (recordValues items)


{-| -}
listOfRecordsMultiline : List (List ( String, String )) -> Int -> String
listOfRecordsMultiline records indent =
    let
        indents =
            newlineWithIndent indent

        items =
            List.map
                (\record_ -> "  " ++ recordMultilineFlat record_ (indent + 1))
                records
    in
    if List.length records == 0 then
        "[]"

    else
        (indents ++ "[ ")
            ++ String.join (indents ++ ", ") items
            ++ (indents ++ "]")


{-| -}
pipelineMultiline : List String -> Int -> String
pipelineMultiline pipedWith indent =
    let
        indents =
            newlineWithIndent (indent + 1)
    in
    String.join (indents ++ "|> ") pipedWith


{-| -}
newlines : String
newlines =
    newline ++ newline


{-| -}
newline : String
newline =
    newlineWithIndent 0


{-| -}
newlineWithIndent : Int -> String
newlineWithIndent indent =
    "\n" ++ prefixIndent indent


tab : String
tab =
    "    "


{-| this is called prefix indent becaue normally you should use newlineWithIndent
-}
prefixIndent : Int -> String
prefixIndent indent =
    String.repeat indent tab


{-| -}
withParens : String -> String
withParens val =
    "(" ++ val ++ ")"


{-| -}
withParensMultiline : String -> Int -> String
withParensMultiline val indent =
    newlineWithIndent indent
        ++ "("
        ++ val
        ++ newlineWithIndent indent
        ++ ")"


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
        , recordMultilineFlat
            [ ( "init", init )
            , ( "view", view )
            , ( "update", update )
            , ( "subscriptions", subscriptions )
            ]
            1
        ]


{-| -}
unstyledView : String -> String
unstyledView view =
    pipelineMultiline [ newlineWithIndent 1 ++ view, "toUnstyled" ] 1


{-| -}
unstyledViewWithIndent : Int -> String -> String
unstyledViewWithIndent indent view =
    pipelineMultiline [ newlineWithIndent indent ++ view, "toUnstyled" ] indent


{-| -}
fromModule : String -> String -> String
fromModule moduleName name =
    moduleName ++ "." ++ name


{-| -}
varWithTypeAnnotation : String -> String -> String -> String
varWithTypeAnnotation name typeValue body =
    varWithTypeMultiline name typeValue body 0


{-| -}
varWithTypeMultiline : String -> String -> String -> Int -> String
varWithTypeMultiline name typeValue body indent =
    prefixIndent indent
        ++ typeAnnotation name typeValue
        ++ newlineWithIndent indent
        ++ var name (indent + 1) body


{-| -}
funcWithType : String -> String -> String -> String -> String
funcWithType name typeValue vars body =
    funcWithTypeMultiline name typeValue vars body 0


{-| -}
funcWithTypeMultiline : String -> String -> String -> String -> Int -> String
funcWithTypeMultiline name typeValue vars body indent =
    prefixIndent indent
        ++ typeAnnotation name typeValue
        ++ newlineWithIndent indent
        ++ var (name ++ " " ++ vars) (indent + 1) body


{-| -}
typeAnnotation : String -> String -> String
typeAnnotation name value =
    name ++ " : " ++ value


{-| -}
var : String -> Int -> String -> String
var varName indent body =
    varName ++ " =" ++ newlineWithIndent indent ++ body ++ "\n"


apply : List String -> String
apply =
    String.join " "


{-| -}
unionType : String -> List String -> String
unionType name constructors =
    "type "
        ++ name
        ++ newlineWithIndent 1
        ++ "= "
        ++ String.join (newlineWithIndent 1 ++ "| ") constructors
