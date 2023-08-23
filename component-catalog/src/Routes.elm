module Routes exposing (Route(..), fromLocation, headerId, toString, updateExample, viewHeader)

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


type Route exampleState exampleMsg usageExampleState usageExampleMsg
    = Doodad (Example exampleState exampleMsg)
    | Category Category.Category
    | CategoryDoodad Category.Category (Example exampleState exampleMsg)
    | Usage (UsageExample usageExampleState usageExampleMsg)
    | All
    | NotFound String


toString : Route state msg usageState usageMsg -> String
toString route_ =
    case route_ of
        Doodad example ->
            "#/doodad/" ++ example.name

        Category c ->
            "#/category/" ++ Category.forRoute c

        CategoryDoodad c example ->
            "#/category_doodad/" ++ Category.forRoute c ++ "/" ++ example.name

        Usage example ->
            "#/usage_example/" ++ UsageExample.routeName example

        All ->
            "#/"

        NotFound unmatchedRoute ->
            unmatchedRoute


route :
    Dict String (Example state msg)
    -> Dict String (UsageExample usageState usageMsg)
    -> Parser (Route state msg usageState usageMsg)
route examples usageExamples =
    let
        findExample :
            (Example state msg -> Route state msg usageState usageMsg)
            -> String
            -> Route state msg usageState usageMsg
        findExample toRoute name =
            Dict.get name examples
                |> Maybe.map toRoute
                |> Maybe.withDefault (NotFound name)

        findUsageExample :
            (UsageExample usageState usageMsg -> Route state msg usageState usageMsg)
            -> String
            -> Route state msg usageState usageMsg
        findUsageExample toRoute name =
            Dict.get (UsageExample.fromRouteName name) usageExamples
                |> Maybe.map toRoute
                |> Maybe.withDefault (NotFound name)
    in
    Parser.oneOf
        [ Parser.succeed (\cat -> findExample (CategoryDoodad cat))
            |. Parser.token "/category_doodad/"
            |= (Parser.getChompedString (Parser.chompWhile ((/=) '/'))
                    |> Parser.andThen category
               )
            |. Parser.token "/"
            |= restOfPath
        , Parser.succeed Category
            |. Parser.token "/category/"
            |= (restOfPath |> Parser.andThen category)
        , Parser.succeed (findExample Doodad)
            |. Parser.token "/doodad/"
            |= restOfPath
        , Parser.succeed (findUsageExample Usage)
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


updateExample : Example state msg -> Route state msg usageState usageMsg -> Maybe (Route state msg usageState usageMsg)
updateExample example route_ =
    case route_ of
        Doodad _ ->
            Just (Doodad example)

        CategoryDoodad cat _ ->
            Just (CategoryDoodad cat example)

        _ ->
            Nothing


fromLocation :
    Dict String (Example state msg)
    -> Dict String (UsageExample usageState usageMsg)
    -> Url
    -> Route state msg usageState usageMsg
fromLocation examples usageExamples location =
    location.fragment
        |> Maybe.withDefault ""
        |> Parser.run (route examples usageExamples)
        |> Result.withDefault All


viewHeader : Route state msg usageState usageMsg -> List (Header.Attribute (Route state msg usageState usageMsg) msg2) -> Html msg2
viewHeader currentRoute extraContent =
    breadCrumbs currentRoute
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


headerId : Route state msg usageState usageMsg -> Maybe String
headerId route_ =
    Maybe.map BreadCrumbs.headerId (breadCrumbs route_)


breadCrumbs : Route state msg usageState usageMsg -> Maybe (BreadCrumbs (Route state msg usageState usageMsg))
breadCrumbs route_ =
    case route_ of
        All ->
            Just allBreadCrumb

        Category category_ ->
            Just (categoryCrumb category_)

        Doodad example ->
            Just (doodadCrumb allBreadCrumb example)

        CategoryDoodad category_ example ->
            Just (doodadCrumb (categoryCrumb category_) example)

        Usage example ->
            Just (usageExampleCrumb example)

        NotFound _ ->
            Nothing


allBreadCrumb : BreadCrumbs (Route state msg usageState usageMsg)
allBreadCrumb =
    BreadCrumbs.init
        { id = "breadcrumbs__all"
        , text = "Component Catalog"
        , route = All
        }
        []


categoryCrumb : Category.Category -> BreadCrumbs (Route state msg usageState usageMsg)
categoryCrumb category_ =
    BreadCrumbs.after allBreadCrumb
        { id = "breadcrumbs__" ++ Category.forId category_
        , text = Category.forDisplay category_
        , route = Category category_
        }
        []


doodadCrumb : BreadCrumbs (Route state msg usageState usageMsg) -> Example state msg -> BreadCrumbs (Route state msg usageState usageMsg)
doodadCrumb previous example =
    BreadCrumbs.after previous
        { id = safeIdWithPrefix "breadcrumbs" example.name
        , text = Example.fullName example
        , route = Doodad example
        }
        []


usageExampleCrumb : UsageExample usageState usageMsg -> BreadCrumbs (Route state msg usageState usageMsg)
usageExampleCrumb example =
    BreadCrumbs.after allBreadCrumb
        { id = safeIdWithPrefix "breadcrumbs" example.name
        , text = UsageExample.fullName example
        , route = Usage example
        }
        []
