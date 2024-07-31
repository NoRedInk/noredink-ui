module Nri.Ui.ElmReview.MemoizedFocusLoopLazy exposing (rule)

{-| This module is shamelessly copied from <https://github.com/NoRedInk/elm-review-html-lazy/blob/master/src/UseMemoizedLazyLambda.elm> and modified for FocusLoop.Lazy.

See the repo above for more details.

@docs rule

-}

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Exposing exposing (Exposing(..))
import Elm.Syntax.Expression exposing (Expression(..), LetDeclaration(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (ContextCreator, Error, Rule)
import Set exposing (Set)


type alias ModuleContext =
    { importedNames : ModuleNameLookupTable
    , importedExposingAll : Set String
    }


type alias KnownModule =
    { name : String
    , functions : Set String
    }


focusLoopLazyModule : KnownModule
focusLoopLazyModule =
    { name = "Nri.Ui.FocusLoop.Lazy.V1"
    , functions = Set.fromList [ "lazy", "lazy2", "lazy3", "lazy4", "lazy5" ]
    }


{-|

    This rule checks that calls to FocusLoop.lazy, lazy2, ... are memoized at the top level of a view function.

-}
rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "UseMemoizedLambda" initialContext
        |> Rule.withImportVisitor importVisitor
        |> Rule.withDeclarationEnterVisitor declarationEnterVisitor
        |> Rule.fromModuleRuleSchema


initialContext : ContextCreator () ModuleContext
initialContext =
    Rule.initContextCreator
        (\importedNames _ ->
            { importedNames = importedNames
            , importedExposingAll = Set.empty
            }
        )
        |> Rule.withModuleNameLookupTable


findLazyCalls : ModuleContext -> Node Expression -> List (Node Expression)
findLazyCalls moduleContext expression =
    fold
        (\exp accum ->
            case identifyLazyFunction moduleContext exp of
                Just _ ->
                    exp :: accum

                _ ->
                    accum
        )
        []
        expression
        |> List.reverse


declarationEnterVisitor : Node Declaration -> ModuleContext -> ( List (Error {}), ModuleContext )
declarationEnterVisitor node moduleContext =
    case Node.value node of
        FunctionDeclaration { declaration } ->
            let
                decl =
                    Node.value declaration

                makeLazyError (Node range _) =
                    Rule.error { message = "Calls to lazy should be memoized at the top level of a view function.", details = [ "See here" ] } range

                errors =
                    case ( normalizeApplication decl.expression, decl.arguments ) of
                        ( [ lazyFunc, _ ], [] ) ->
                            case identifyLazyFunction moduleContext lazyFunc of
                                Just _ ->
                                    []

                                Nothing ->
                                    findLazyCalls moduleContext decl.expression
                                        |> List.map makeLazyError

                        _ ->
                            findLazyCalls moduleContext decl.expression
                                |> List.map makeLazyError
            in
            ( errors, moduleContext )

        _ ->
            ( [], moduleContext )


importVisitor :
    Node Import
    -> { context | importedExposingAll : Set String }
    -> ( List (Error {}), { context | importedExposingAll : Set String } )
importVisitor (Node _ { moduleName, exposingList }) context =
    case exposingList of
        Just (Node _ (All _)) ->
            ( [], { context | importedExposingAll = Set.insert (Node.value moduleName |> String.join ".") context.importedExposingAll } )

        _ ->
            ( [], context )


identifyLazyFunction :
    { context | importedNames : ModuleNameLookupTable, importedExposingAll : Set String }
    -> Node Expression
    -> Maybe String
identifyLazyFunction { importedNames, importedExposingAll } node =
    case Node.value node of
        FunctionOrValue _ functionName ->
            case ModuleNameLookupTable.moduleNameFor importedNames node of
                Just ((_ :: _) as moduleNameList) ->
                    let
                        moduleName =
                            moduleNameList |> String.join "."

                        isLazyModule =
                            moduleName == focusLoopLazyModule.name
                    in
                    if isLazyModule then
                        Just functionName

                    else
                        Nothing

                _ ->
                    let
                        fromHtmlLazy =
                            Set.member focusLoopLazyModule.name importedExposingAll && Set.member functionName focusLoopLazyModule.functions
                    in
                    if fromHtmlLazy then
                        Just functionName

                    else
                        Nothing

        _ ->
            Nothing



{- https://github.com/NoRedInk/elm-review-html-lazy/blob/master/src/Elm/Syntax/Expression/Extra.elm -}


foldHelper : (Node Expression -> a -> a) -> a -> List (Node Expression) -> a
foldHelper function accum stack =
    case stack of
        [] ->
            accum

        expr :: stackTail ->
            let
                newStack =
                    case Node.value expr of
                        Application exprs ->
                            exprs

                        OperatorApplication _ _ leftExp rightExp ->
                            [ leftExp, rightExp ]

                        IfBlock condExp trueExp falseExp ->
                            [ condExp, trueExp, falseExp ]

                        Negation exp ->
                            [ exp ]

                        TupledExpression exps ->
                            exps

                        ParenthesizedExpression exp ->
                            [ exp ]

                        LetExpression { declarations, expression } ->
                            let
                                mapLetDeclarations (Node _ letDeclaration) =
                                    case letDeclaration of
                                        LetFunction { declaration } ->
                                            (Node.value declaration).expression

                                        LetDestructuring _ exp ->
                                            exp
                            in
                            List.map mapLetDeclarations declarations ++ [ expression ]

                        CaseExpression { expression, cases } ->
                            expression :: List.map Tuple.second cases

                        LambdaExpression { expression } ->
                            [ expression ]

                        RecordExpr recordSetters ->
                            List.map (Node.value >> Tuple.second) recordSetters

                        ListExpr exps ->
                            exps

                        RecordAccess exp _ ->
                            [ exp ]

                        RecordUpdateExpression _ recordSetters ->
                            List.map (Node.value >> Tuple.second) recordSetters

                        _ ->
                            []
            in
            foldHelper function (function expr accum) (newStack ++ stackTail)


fold : (Node Expression -> a -> a) -> a -> Node Expression -> a
fold function accum expr =
    foldHelper function accum [ expr ]


unParenthesize : Node Expression -> Node Expression
unParenthesize node =
    case Node.value node of
        ParenthesizedExpression exp ->
            unParenthesize exp

        _ ->
            node


normalizeApplicationHelper : Node Expression -> List (Node Expression) -> List (Node Expression)
normalizeApplicationHelper exp accum =
    case Node.value exp of
        Application (func :: args) ->
            normalizeApplicationHelper func (args ++ accum)

        OperatorApplication "<|" _ func arg ->
            normalizeApplicationHelper func (arg :: accum)

        OperatorApplication "|>" _ arg func ->
            normalizeApplicationHelper func (arg :: accum)

        ParenthesizedExpression innerExp ->
            normalizeApplicationHelper innerExp accum

        _ ->
            exp :: List.map unParenthesize accum


normalizeApplication : Node Expression -> List (Node Expression)
normalizeApplication exp =
    normalizeApplicationHelper exp []
