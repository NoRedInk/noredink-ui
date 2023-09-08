module Routes exposing
    ( Route(..), toString, fromLocation
    , viewHeader, headerId
    , exampleRoute
    , exampleHref, usageExampleHref
    )

{-|

@docs Route, toString, fromLocation
@docs viewHeader, headerId
@docs exampleRoute
@docs exampleHref, usageExampleHref

-}

import Accessibility.Styled as Html exposing (Html)
import Category
import Css
import Dict exposing (Dict)
import Example exposing (Example)
import Html.Styled.Attributes as Attributes
import Nri.Ui.BreadCrumbs.V2 as BreadCrumbs exposing (BreadCrumbs)
import Nri.Ui.Header.V1 as Header
import Nri.Ui.Html.Attributes.V2 exposing (safeIdWithPrefix)
import Parser exposing ((|.), (|=), Parser)
import Url exposing (Url)
import UsageExample exposing (UsageExample)


type Route
    = Doodad String
    | Category Category.Category
    | CategoryDoodad Category.Category String
    | Usage String
    | All


toString : Route -> String
toString route_ =
    case route_ of
        Doodad exampleName ->
            "#/doodad/" ++ exampleName

        Category c ->
            "#/category/" ++ Category.forRoute c

        CategoryDoodad c exampleName ->
            "#/category_doodad/" ++ Category.forRoute c ++ "/" ++ exampleName

        Usage exampleName ->
            "#/usage_example/" ++ exampleName

        All ->
            "#/"


route : Parser Route
route =
    Parser.oneOf
        [ Parser.succeed CategoryDoodad
            |. Parser.token "/category_doodad/"
            |= (Parser.getChompedString (Parser.chompWhile ((/=) '/'))
                    |> Parser.andThen category
               )
            |. Parser.token "/"
            |= restOfPath
        , Parser.succeed Category
            |. Parser.token "/category/"
            |= (restOfPath |> Parser.andThen category)
        , Parser.succeed Doodad
            |. Parser.token "/doodad/"
            |= restOfPath
        , Parser.succeed Usage
            |. Parser.token "/usage_example/"
            |= restOfPath
        , Parser.succeed All
        ]


restOfPath : Parser String
restOfPath =
    Parser.getChompedString (Parser.chompWhile (always True))


category : String -> Parser Category.Category
category string =
    case Category.fromString string of
        Ok c ->
            Parser.succeed c

        Err e ->
            Parser.problem e


fromLocation : Url -> Route
fromLocation location =
    location.fragment
        |> Maybe.withDefault ""
        |> Parser.run route
        |> Result.withDefault All


viewHeader :
    Route
    -> Dict String (Example state msg)
    -> Dict String (UsageExample usageState usageMsg)
    -> List (Header.Attribute Route msg2)
    -> Html msg2
viewHeader currentRoute examples usageExamples extraContent =
    breadCrumbs currentRoute examples usageExamples
        |> Maybe.map
            (\crumbs ->
                Header.view
                    ([ Header.aTagAttributes (\r -> [ Attributes.href ("/" ++ toString r) ])
                     , Header.customPageWidth (Css.px 1400)
                     ]
                        ++ extraContent
                    )
                    { breadCrumbs = crumbs
                    , isCurrentRoute = (==) currentRoute
                    }
            )
        |> Maybe.withDefault (Html.text "")


headerId :
    Route
    -> Dict String (Example state msg)
    -> Dict String (UsageExample usageState usageMsg)
    -> Maybe String
headerId route_ examples usageExamples =
    Maybe.map BreadCrumbs.headerId (breadCrumbs route_ examples usageExamples)


breadCrumbs :
    Route
    -> Dict String (Example state msg)
    -> Dict String (UsageExample usageState usageMsg)
    -> Maybe (BreadCrumbs Route)
breadCrumbs route_ examples usageExamples =
    case route_ of
        All ->
            Just allBreadCrumb

        Category category_ ->
            Just (categoryCrumb category_)

        Doodad exampleName ->
            Maybe.map (doodadCrumb allBreadCrumb) (Dict.get exampleName examples)

        CategoryDoodad category_ exampleName ->
            Maybe.map
                (\example ->
                    doodadCrumb (categoryCrumb category_) example
                )
                (Dict.get exampleName examples)

        Usage exampleName ->
            Maybe.map usageExampleCrumb (Dict.get exampleName usageExamples)


allBreadCrumb : BreadCrumbs Route
allBreadCrumb =
    BreadCrumbs.init
        { id = "breadcrumbs__all"
        , text = "Component Catalog"
        , route = All
        }
        []


categoryCrumb : Category.Category -> BreadCrumbs Route
categoryCrumb category_ =
    BreadCrumbs.after allBreadCrumb
        { id = "breadcrumbs__" ++ Category.forId category_
        , text = Category.forDisplay category_
        , route = Category category_
        }
        []


doodadCrumb : BreadCrumbs Route -> Example state msg -> BreadCrumbs Route
doodadCrumb previous example =
    BreadCrumbs.after previous
        { id = safeIdWithPrefix "breadcrumbs" example.name
        , text = Example.fullName example
        , route = Doodad (Example.routeName example)
        }
        []


usageExampleCrumb : UsageExample a b -> BreadCrumbs Route
usageExampleCrumb example =
    BreadCrumbs.after allBreadCrumb
        { id = safeIdWithPrefix "breadcrumbs" example.name
        , text = UsageExample.fullName example
        , route = Usage (UsageExample.routeName example)
        }
        []


exampleRoute : Example a b -> Route
exampleRoute example =
    Doodad (Example.routeName example)


exampleHref : Example a b -> String
exampleHref =
    exampleRoute >> toString


usageExampleRoute : UsageExample a b -> Route
usageExampleRoute example =
    Usage (UsageExample.routeName example)


usageExampleHref : UsageExample a b -> String
usageExampleHref =
    usageExampleRoute >> toString
