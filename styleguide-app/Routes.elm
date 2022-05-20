module Routes exposing (Route(..), fromLocation, toString, viewBreadCrumbs)

import Accessibility.Styled as Html exposing (Html)
import Category
import Html.Styled.Attributes as Attributes
import Nri.Ui.BreadCrumbs.V1 as BreadCrumbs exposing (BreadCrumbs)
import Parser exposing ((|.), (|=), Parser)
import Url exposing (Url)


type Route
    = Doodad String
    | Category Category.Category
    | All


toString : Route -> String
toString route_ =
    case route_ of
        Doodad exampleName ->
            "#/doodad/" ++ exampleName

        Category c ->
            "#/category/" ++ Debug.toString c

        All ->
            "#/"


route : Parser Route
route =
    Parser.oneOf
        [ Parser.succeed Category
            |. Parser.token "/category/"
            |= (restOfPath |> Parser.andThen category)
        , Parser.succeed Doodad
            |. Parser.token "/doodad/"
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


viewBreadCrumbs : Route -> Html msg
viewBreadCrumbs currentRoute =
    breadCrumbs currentRoute
        |> Maybe.map
            (BreadCrumbs.view
                { aTagAttributes = \r -> [ Attributes.href ("/" ++ toString r) ]
                , isCurrentRoute = (==) currentRoute
                }
            )
        |> Maybe.withDefault (Html.text "")


breadCrumbs : Route -> Maybe (BreadCrumbs Route)
breadCrumbs route_ =
    case route_ of
        All ->
            Just allBreadCrumb

        Category category_ ->
            Just (categoryCrumb category_)

        Doodad _ ->
            Nothing


allBreadCrumb : BreadCrumbs Route
allBreadCrumb =
    BreadCrumbs.init
        { icon = Nothing
        , iconStyle = BreadCrumbs.Default
        , id = "breadcrumbs__all"
        , text = "All"
        , route = All
        }


categoryCrumb : Category.Category -> BreadCrumbs Route
categoryCrumb category_ =
    BreadCrumbs.after allBreadCrumb
        { icon = Nothing
        , iconStyle = BreadCrumbs.Default
        , id = "breadcrumbs__" ++ Category.forId category_
        , text = Category.forDisplay category_
        , route = Category category_
        }
