module Routes exposing (Route(..), fromLocation, headerId, toString, updateExample, viewHeader)

import Accessibility.Styled as Html exposing (Html)
import Category
import Css
import Dict exposing (Dict)
import Example exposing (Example)
import Html.Styled.Attributes as Attributes
import Nri.Ui.BreadCrumbs.V2 as BreadCrumbs exposing (BreadCrumbs)
import Nri.Ui.Header.V1 as Header
import Nri.Ui.Util exposing (dashify)
import Parser exposing ((|.), (|=), Parser)
import Url exposing (Url)


type Route state msg
    = Doodad (Example state msg)
    | Category Category.Category
    | CategoryDoodad Category.Category (Example state msg)
    | All
    | NotFound String


toString : Route state msg -> String
toString route_ =
    case route_ of
        Doodad example ->
            "#/doodad/" ++ example.name

        Category c ->
            "#/category/" ++ Category.forRoute c

        CategoryDoodad c example ->
            "#/category_doodad/" ++ Category.forRoute c ++ "/" ++ example.name

        All ->
            "#/"

        NotFound unmatchedRoute ->
            unmatchedRoute


route : Dict String (Example state msg) -> Parser (Route state msg)
route examples =
    let
        findExample : (Example state msg -> Route state msg) -> String -> Route state msg
        findExample toRoute name =
            Dict.get name examples
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


updateExample : Example state msg -> Route state msg -> Maybe (Route state msg)
updateExample example route_ =
    case route_ of
        Doodad _ ->
            Just (Doodad example)

        CategoryDoodad cat _ ->
            Just (CategoryDoodad cat example)

        _ ->
            Nothing


fromLocation : Dict String (Example state msg) -> Url -> Route state msg
fromLocation examples location =
    location.fragment
        |> Maybe.withDefault ""
        |> Parser.run (route examples)
        |> Result.withDefault All


viewHeader : Route state msg -> List (Header.Attribute (Route state msg) msg2) -> Html msg2
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


headerId : Route state msg -> Maybe String
headerId route_ =
    Maybe.map BreadCrumbs.headerId (breadCrumbs route_)


breadCrumbs : Route state msg -> Maybe (BreadCrumbs (Route state msg))
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

        NotFound _ ->
            Nothing


allBreadCrumb : BreadCrumbs (Route state msg)
allBreadCrumb =
    BreadCrumbs.init
        { id = "breadcrumbs__all"
        , text = "Component Catalog"
        , route = All
        }
        []


categoryCrumb : Category.Category -> BreadCrumbs (Route state msg)
categoryCrumb category_ =
    BreadCrumbs.after allBreadCrumb
        { id = "breadcrumbs__" ++ Category.forId category_
        , text = Category.forDisplay category_
        , route = Category category_
        }
        []


doodadCrumb : BreadCrumbs (Route state msg) -> Example state msg -> BreadCrumbs (Route state msg)
doodadCrumb previous example =
    BreadCrumbs.after previous
        { id = "breadcrumbs__" ++ dashify example.name
        , text = Example.fullName example
        , route = Doodad example
        }
        []
