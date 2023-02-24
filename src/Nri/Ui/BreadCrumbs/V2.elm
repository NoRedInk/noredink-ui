module Nri.Ui.BreadCrumbs.V2 exposing
    ( BreadCrumbs, init, initSecondary, after
    , BreadCrumbAttribute, icon, iconSize
    , view, viewSecondary
    , headerId, toPageTitle
    )

{-|


# Changes from V1:

  - switch to list-based attributes pattern

Learn more about 'breadcrumbs' to help a user orient themselves within a site here: <https://www.w3.org/WAI/WCAG21/Techniques/general/G65>.

Wide Viewport:

    Home

    🏠 Home > 🟠 Category 1

    🏠 > 🟠 Category 1 > 🟣 Sub-Category 2

Narrow Viewport:

    Home

    🏠 > 🟠 Category 1

    🏠 > 🟠 > 🟣 Sub-Category 2


## Creating breadcrumbs

@docs BreadCrumbs, init, initSecondary, after
@docs BreadCrumbAttribute, icon, iconSize


## Viewing breadcrumbs

@docs view, viewSecondary


## Managing focus and page title

@docs headerId, toPageTitle

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Style as Style
import Css exposing (..)
import Css.Media as Media
import Html.Styled
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
type BreadCrumb route
    = BreadCrumb (BreadCrumbAttributes route)


{-| -}
create :
    { id : String, text : String, route : route }
    -> List (BreadCrumbAttribute route)
    -> BreadCrumb route
create { id, text, route } optionalAttributes =
    BreadCrumb
        (List.foldl (\(BreadCrumbAttribute attribute) b -> attribute b)
            { icon = Nothing
            , iconSize = Css.px 31
            , id = id
            , text = text
            , route = route
            }
            optionalAttributes
        )


{-| -}
icon : Svg -> BreadCrumbAttribute route
icon icon_ =
    BreadCrumbAttribute (\attributes -> { attributes | icon = Just icon_ })


{-| -}
iconSize : Css.Px -> BreadCrumbAttribute route
iconSize custom =
    BreadCrumbAttribute (\attributes -> { attributes | iconSize = custom })


{-| -}
type BreadCrumbAttribute route
    = BreadCrumbAttribute (BreadCrumbAttributes route -> BreadCrumbAttributes route)


type alias BreadCrumbAttributes route =
    { icon : Maybe Svg.Svg
    , iconSize : Css.Px
    , id : String
    , text : String
    , route : route
    }


{-| -}
type BreadCrumbs route
    = BreadCrumbs
        { primary : List (BreadCrumb route)
        , secondary : List (BreadCrumb route)
        }


{-| -}
init :
    { id : String, text : String, route : route }
    -> List (BreadCrumbAttribute route)
    -> BreadCrumbs route
init required optional =
    BreadCrumbs { primary = [ create required optional ], secondary = [] }


{-| -}
after :
    BreadCrumbs route
    -> { id : String, text : String, route : route }
    -> List (BreadCrumbAttribute route)
    -> BreadCrumbs route
after (BreadCrumbs crumbs) required optional =
    case crumbs.secondary of
        [] ->
            BreadCrumbs { crumbs | primary = create required optional :: crumbs.primary }

        _ ->
            BreadCrumbs { crumbs | secondary = create required optional :: crumbs.secondary }


{-| -}
initSecondary :
    BreadCrumbs route
    -> { id : String, text : String, route : route }
    -> List (BreadCrumbAttribute route)
    -> BreadCrumbs route
initSecondary (BreadCrumbs crumbs) required optional =
    BreadCrumbs { crumbs | secondary = [ create required optional ] }


{-| -}
headerId : BreadCrumbs route -> String
headerId (BreadCrumbs { primary, secondary }) =
    let
        extract (BreadCrumb crumb) =
            crumb
    in
    List.map extract secondary
        ++ List.map extract primary
        |> List.head
        |> Maybe.map .id
        |> Maybe.withDefault ""


{-| Generate an HTML page title using the breadcrumbs, in the form "SubCategory | Category | NoRedInk" for breadCrumbs like:

    Category > SubCategory

-}
toPageTitle : BreadCrumbs a -> String
toPageTitle (BreadCrumbs { primary, secondary }) =
    let
        primaryPageComponents =
            if List.isEmpty secondary then
                List.map pageTitle primary

            else
                List.head primary
                    |> Maybe.map (pageTitle >> List.singleton)
                    |> Maybe.withDefault []
    in
    List.map pageTitle secondary
        ++ primaryPageComponents
        ++ [ "NoRedInk" ]
        |> String.join " | "


pageTitle : BreadCrumb a -> String
pageTitle (BreadCrumb { text }) =
    text


type Level
    = H1
    | H2


viewLevel : Level -> (List (Attribute msg) -> List (Html msg) -> Html msg)
viewLevel heading =
    case heading of
        H1 ->
            Html.Styled.h1

        H2 ->
            Html.Styled.h2


fontCss : Level -> List Style
fontCss heading =
    case heading of
        H1 ->
            [ fontSize (px 30)
            , Media.withMedia [ MediaQuery.mobile ] [ fontSize (px 25) ]
            , color Colors.navy
            ]

        H2 ->
            [ fontSize (px 20)
            , color Colors.navy
            ]


linkCss : Level -> List Style
linkCss heading =
    case heading of
        H1 ->
            []

        H2 ->
            [ color Colors.azure
            , hover [ color Colors.azureDark ]
            ]


{-| Usually, the label value will be the string "breadcrumbs".

It's configurable so that if more than one set of BreadCrumbs ever appear on the page, the aria-label for the nav can still be unique.

-}
view :
    { aTagAttributes : route -> List (Attribute msg)
    , isCurrentRoute : route -> Bool
    , label : String
    }
    -> BreadCrumbs route
    -> Html msg
view config (BreadCrumbs { primary }) =
    viewNavOrH1 H1 config (List.reverse primary)


{-| Usually, the label value will be the string "secondary breadcrumbs".

It's configurable so that if more than one set of BreadCrumbs ever appear on the page, the aria-label for the nav can still be unique.

-}
viewSecondary :
    { aTagAttributes : route -> List (Attribute msg)
    , isCurrentRoute : route -> Bool
    , label : String
    }
    -> BreadCrumbs route
    -> Html msg
viewSecondary config (BreadCrumbs { secondary }) =
    viewNavOrH1 H2 config (List.reverse secondary)


viewNavOrH1 :
    Level
    ->
        { aTagAttributes : route -> List (Attribute msg)
        , isCurrentRoute : route -> Bool
        , label : String
        }
    -> List (BreadCrumb route)
    -> Html msg
viewNavOrH1 headingLevel config breadCrumbs =
    let
        breadCrumbCount : Int
        breadCrumbCount =
            List.length breadCrumbs

        renderedBreadCrumbs =
            List.indexedMap
                (\i ->
                    viewBreadCrumb
                        headingLevel
                        config
                        { index = i
                        , finalIndex = breadCrumbCount - 1
                        , isIconOnly =
                            -- the first breadcrumb should collapse when there
                            -- are 3 breadcrumbs or more in the group
                            --
                            -- Hypothetically, if there were 4 breadcrumbs, then the
                            -- first 2 breadcrumbs should collapse
                            (breadCrumbCount - i) > 2
                        }
                )
                breadCrumbs
                |> List.intersperse (Svg.toHtml arrowRight)
    in
    if breadCrumbCount == 1 then
        div [ css navContainerStyles ]
            renderedBreadCrumbs

    else
        nav [ Aria.label config.label, css navContainerStyles ]
            renderedBreadCrumbs


navContainerStyles : List Css.Style
navContainerStyles =
    [ alignItems center
    , displayFlex
    , Media.withMedia [ MediaQuery.mobile ] [ marginBottom (px 10), flexWrap wrap ]
    ]


viewBreadCrumb :
    Level
    ->
        { config
            | aTagAttributes : route -> List (Attribute msg)
            , isCurrentRoute : route -> Bool
        }
    -> { index : Int, finalIndex : Int, isIconOnly : Bool }
    -> BreadCrumb route
    -> Html msg
viewBreadCrumb headingLevel config { index, finalIndex, isIconOnly } (BreadCrumb crumb) =
    let
        commonCss =
            [ alignItems center
            , displayFlex
            , margin zero
            , Css.batch (fontCss headingLevel)
            , Fonts.baseFont
            , textDecoration none
            ]

        isLast =
            index == finalIndex

        content =
            viewBreadCrumbContent { isLast = isLast, isIconOnly = isIconOnly } crumb
    in
    if isLast then
        viewLevel headingLevel
            [ AttributesExtra.includeIf (index /= 0) Aria.currentPage
            , Attributes.id crumb.id
            , Attributes.tabindex -1
            , css (fontWeight bold :: commonCss)
            ]
            (if config.isCurrentRoute crumb.route then
                content

             else
                [ Html.Styled.a
                    (css (commonCss ++ linkCss headingLevel)
                        :: config.aTagAttributes crumb.route
                    )
                    content
                ]
            )

    else
        Html.Styled.a
            (css (commonCss ++ linkCss headingLevel)
                :: Attributes.id crumb.id
                :: config.aTagAttributes crumb.route
            )
            content


viewBreadCrumbContent :
    { isLast : Bool, isIconOnly : Bool }
    -> BreadCrumbAttributes route
    -> List (Html msg)
viewBreadCrumbContent iconConfig crumb =
    case crumb.icon of
        Just icon_ ->
            [ viewIcon icon_ crumb.iconSize
            , viewHeadingWithIcon iconConfig crumb.text
            ]

        Nothing ->
            [ text crumb.text ]


viewHeadingWithIcon : { isLast : Bool, isIconOnly : Bool } -> String -> Html msg
viewHeadingWithIcon { isIconOnly, isLast } title =
    span
        (if isIconOnly then
            Style.invisible

         else if isLast then
            [ css [ marginLeft horizontalSpacing ] ]

         else
            [ css
                [ marginLeft horizontalSpacing
                , Media.withMedia [ MediaQuery.mobile ]
                    [ Style.invisibleStyle
                    ]
                ]
            ]
        )
        [ text title
        ]


viewIcon : Svg.Svg -> Css.Px -> Html msg
viewIcon icon_ size =
    icon_
        |> Svg.withWidth size
        |> Svg.withHeight size
        |> Svg.withCss [ Css.flexShrink Css.zero ]
        |> Svg.toHtml


horizontalSpacing : Css.Px
horizontalSpacing =
    Css.px 10


arrowRight : Svg.Svg
arrowRight =
    UiIcon.arrowRight
        |> Svg.withColor Colors.gray75
        |> Svg.withHeight (px 15)
        |> Svg.withWidth (px 15)
        |> Svg.withCss
            [ marginRight horizontalSpacing
            , marginLeft horizontalSpacing
            , flexShrink zero
            ]
