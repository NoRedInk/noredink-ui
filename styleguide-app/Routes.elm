module Routes exposing (Route(..), fromLocation, toString, viewBreadCrumbs)

import Accessibility.Styled as Html exposing (Html)
import Category
import Dict exposing (Dict)
import Example exposing (Example)
import Html.Styled.Attributes as Attributes
import Nri.Ui.BreadCrumbs.V1 as BreadCrumbs exposing (BreadCrumbs)
import Nri.Ui.Util exposing (dashify)
import Parser exposing ((|.), (|=), Parser)
import Url exposing (Url)


type Route state msg
    = Doodad (Example state msg)
    | Category Category.Category
    | All
    | NotFound String


toString : Route state msg -> String
toString route_ =
    case route_ of
        NotFound name ->
            "#/doodad/" ++ name

        Doodad example ->
            "#/doodad/" ++ example.name

        Category c ->
            "#/category/" ++ Debug.toString c

        All ->
            "#/"


route : Dict String (Example state msg) -> Parser (Route state msg)
route examples =
    let
        findExample : String -> Route state msg
        findExample name =
            Dict.get name examples
                |> Maybe.map Doodad
                |> Maybe.withDefault (NotFound name)
    in
    Parser.oneOf
        [ Parser.succeed Category
            |. Parser.token "/category/"
            |= (restOfPath |> Parser.andThen category)
        , Parser.succeed findExample
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


fromLocation : Dict String (Example state msg) -> Url -> Route state msg
fromLocation examples location =
    location.fragment
        |> Maybe.withDefault ""
        |> Parser.run (route examples)
        |> Result.withDefault All


viewBreadCrumbs : Route state msg -> Html msg2
viewBreadCrumbs currentRoute =
    breadCrumbs currentRoute
        |> Maybe.map
            (BreadCrumbs.view
                { aTagAttributes = \r -> [ Attributes.href ("/" ++ toString r) ]
                , isCurrentRoute = (==) currentRoute
                }
            )
        |> Maybe.withDefault (Html.text "")


breadCrumbs : Route state msg -> Maybe (BreadCrumbs (Route state msg))
breadCrumbs route_ =
    case route_ of
        All ->
            Just allBreadCrumb

        Category category_ ->
            Just (categoryCrumb category_)

        Doodad example ->
            Just (doodadCrumb example)

        NotFound _ ->
            Nothing


allBreadCrumb : BreadCrumbs (Route state msg)
allBreadCrumb =
    BreadCrumbs.init
        { icon = Nothing
        , iconStyle = BreadCrumbs.Default
        , id = "breadcrumbs__all"
        , text = "All"
        , route = All
        }


categoryCrumb : Category.Category -> BreadCrumbs (Route state msg)
categoryCrumb category_ =
    BreadCrumbs.after allBreadCrumb
        { icon = Nothing
        , iconStyle = BreadCrumbs.Default
        , id = "breadcrumbs__" ++ Category.forId category_
        , text = Category.forDisplay category_
        , route = Category category_
        }


doodadCrumb : Example state msg -> BreadCrumbs (Route state msg)
doodadCrumb example =
    BreadCrumbs.after allBreadCrumb
        { icon = Nothing
        , iconStyle = BreadCrumbs.Default
        , id = "breadcrumbs__" ++ dashify example.name
        , text = Example.fullName example
        , route = Doodad example
        }
