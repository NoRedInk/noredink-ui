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
import Css.Global
import Css.Media as Media
import Html.Styled
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
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
headerId : BreadCrumbs route -> Maybe String
headerId (BreadCrumbs { primary, secondary }) =
    let
        extract (BreadCrumb crumb) =
            crumb
    in
    List.map extract secondary
        ++ List.map extract primary
        |> List.head
        |> Maybe.map .id


{-| TODO: implement <https://noredink.slack.com/archives/C71TD8MUY/p1662584306753169?thread_ts=1659978195.802739&cid=C71TD8MUY>

Generate an HTML page title using the breadcrumbs, in the form "SubCategory | Category | NoRedInk" for breadCrumbs like:

    Category > SubCategory

-}
toPageTitle : BreadCrumbs a -> String
toPageTitle (BreadCrumbs { primary, secondary }) =
    List.map pageTitle secondary
        ++ List.map pageTitle primary
        ++ [ "NoRedInk" ]
        |> String.join " | "


pageTitle : BreadCrumb a -> String
pageTitle (BreadCrumb { text }) =
    text


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
    viewBreadCrumbs Html.Styled.h1 config (List.reverse primary)
        |> navContainer config.label


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
    viewBreadCrumbs Html.Styled.h2 config (List.reverse secondary)
        |> navContainer config.label


navContainer : String -> List (Html msg) -> Html msg
navContainer label =
    styled nav
        [ alignItems center
        , displayFlex
        , flexWrap wrap
        , Media.withMedia [ MediaQuery.mobile ] [ marginBottom (px 10) ]
        ]
        [ Aria.label label ]


viewBreadCrumbs :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    ->
        { config
            | aTagAttributes : route -> List (Attribute msg)
            , isCurrentRoute : route -> Bool
        }
    -> List (BreadCrumb route)
    -> List (Html msg)
viewBreadCrumbs headingLevel config breadCrumbs =
    let
        breadCrumbCount : Int
        breadCrumbCount =
            List.length breadCrumbs
    in
    List.indexedMap
        (\i ->
            viewBreadCrumb headingLevel
                config
                { isLast = (i + 1) == breadCrumbCount
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


viewBreadCrumb :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    ->
        { config
            | aTagAttributes : route -> List (Attribute msg)
            , isCurrentRoute : route -> Bool
        }
    -> { isLast : Bool, isIconOnly : Bool }
    -> BreadCrumb route
    -> Html msg
viewBreadCrumb headingLevel config iconConfig (BreadCrumb crumb) =
    let
        content =
            viewBreadCrumbContent iconConfig crumb
    in
    if iconConfig.isLast then
        heading headingLevel crumb.id <|
            if config.isCurrentRoute crumb.route then
                content

            else
                [ Html.Styled.a
                    (css commonCss
                        :: config.aTagAttributes crumb.route
                    )
                    content
                ]

    else
        Html.Styled.a
            (css (fontWeight normal :: commonCss)
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


heading :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> String
    -> List (Html msg)
    -> Html msg
heading heading_ id =
    heading_
        [ Aria.currentPage
        , Attributes.id id
        , Attributes.tabindex -1
        , css (fontWeight bold :: commonCss)
        ]


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


commonCss : List Style
commonCss =
    [ alignItems center
    , displayFlex
    , margin zero
    , fontSize (px 30)
    , Media.withMedia [ MediaQuery.mobile ] [ fontSize (px 25) ]
    , Fonts.baseFont
    , textDecoration none
    , color Colors.navy
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
