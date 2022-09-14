module Nri.Ui.BreadCrumbs.V2 exposing
    ( BreadCrumbs, init, after
    , BreadCrumbAttribute, icon, iconCircledStyle
    , view, viewSecondary
    , headerId, toPageTitle
    )

{-|


# Changes from V1:

  - switch to list-based attributes pattern

Learn more about 'breadcrumbs' to help a user orient themselves within a site here: <https://www.w3.org/WAI/WCAG21/Techniques/general/G65>.

Wide Viewport (with Circled IconStyle):

    Home

    🏠 Home > 🟠 Category 1

    🏠 > 🟠 Category 1 > 🟣 Sub-Category 2

Narrow Viewport (with Circled IconStyle):

    Home

    🏠 > 🟠 Category 1

    🏠 > 🟠 > 🟣 Sub-Category 2


## Creating breadcrumbs

@docs BreadCrumbs, init, after
@docs BreadCrumbAttribute, icon, iconCircledStyle


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
            , iconStyle = Default
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
iconCircledStyle : Bool -> BreadCrumbAttribute route
iconCircledStyle circled =
    BreadCrumbAttribute
        (\attributes ->
            { attributes
                | iconStyle =
                    if circled then
                        Circled

                    else
                        Default
            }
        )


{-| -}
type BreadCrumbAttribute route
    = BreadCrumbAttribute (BreadCrumbAttributes route -> BreadCrumbAttributes route)


type alias BreadCrumbAttributes route =
    { icon : Maybe Svg.Svg
    , iconStyle : IconStyle
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


{-| -}
type IconStyle
    = Circled
    | Default


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
    navContainer config.label (viewBreadCrumbs config (List.reverse primary))


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
    navContainer config.label (viewBreadCrumbs config (List.reverse secondary))


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
    { config
        | aTagAttributes : route -> List (Attribute msg)
        , isCurrentRoute : route -> Bool
    }
    -> List (BreadCrumb route)
    -> List (Html msg)
viewBreadCrumbs config breadCrumbs =
    let
        breadCrumbCount : Int
        breadCrumbCount =
            List.length breadCrumbs
    in
    List.indexedMap
        (\i ->
            viewBreadCrumb config
                { isFirst = i == 0
                , isLast = (i + 1) == breadCrumbCount
                , isIconOnly =
                    -- the first breadcrumb should collapse when there
                    -- are 3 breadcrumbs or more
                    --
                    -- Hypothetically, if there were 4 breadcrumbs, then the
                    -- first 2 breadcrumbs should collapse
                    (breadCrumbCount - i) > 2
                }
        )
        breadCrumbs
        |> List.intersperse (Svg.toHtml arrowRight)


viewBreadCrumb :
    { config
        | aTagAttributes : route -> List (Attribute msg)
        , isCurrentRoute : route -> Bool
    }
    -> { isFirst : Bool, isLast : Bool, isIconOnly : Bool }
    -> BreadCrumb route
    -> Html msg
viewBreadCrumb config iconConfig (BreadCrumb crumb) =
    let
        isLink =
            not (config.isCurrentRoute crumb.route)

        linkAttrs =
            if isLink then
                css
                    [ hover
                        [ Css.Global.descendants
                            [ Css.Global.class circleIconClass
                                [ backgroundColor Colors.glacier
                                , borderColor Colors.azureDark
                                , color Colors.azure
                                ]
                            ]
                        ]
                    ]
                    :: config.aTagAttributes crumb.route

            else
                []

        withIconIfPresent viewIcon =
            case crumb.icon of
                Just icon_ ->
                    [ viewIcon iconConfig.isFirst crumb.iconStyle icon_
                    , viewHeadingWithIcon iconConfig crumb.text
                    ]

                Nothing ->
                    [ text crumb.text ]
    in
    case ( iconConfig.isLast, isLink ) of
        ( True, False ) ->
            pageHeader crumb.id
                (withIconIfPresent viewIconForHeading)

        ( True, True ) ->
            pageHeader crumb.id
                [ Html.Styled.styled Html.Styled.a
                    []
                    (css commonCss :: linkAttrs)
                    (withIconIfPresent viewIconForLink)
                ]

        ( False, _ ) ->
            Html.Styled.styled Html.Styled.a
                [ fontWeight normal ]
                (css commonCss :: Attributes.id crumb.id :: linkAttrs)
                (withIconIfPresent viewIconForLink)


pageHeader : String -> List (Html msg) -> Html msg
pageHeader id =
    styled h1
        [ fontWeight bold ]
        [ Aria.currentPage
        , Attributes.id id
        , Attributes.tabindex -1
        , css commonCss
        ]


viewIconForHeading : Bool -> IconStyle -> Svg.Svg -> Html msg
viewIconForHeading isFirst iconStyle svg =
    case iconStyle of
        Circled ->
            text ""

        Default ->
            withoutIconCircle isFirst svg


viewIconForLink : Bool -> IconStyle -> Svg.Svg -> Html msg
viewIconForLink isFirst iconStyle svg =
    case iconStyle of
        Circled ->
            withIconCircle svg

        Default ->
            withoutIconCircle isFirst svg


viewHeadingWithIcon : { config | isLast : Bool, isIconOnly : Bool } -> String -> Html msg
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


circleIconClass : String
circleIconClass =
    "Nri-BreadCrumb-base-circled-icon"


withIconCircle : Svg.Svg -> Html msg
withIconCircle icon_ =
    styled span
        [ borderRadius (pct 50)
        , border3 (px 1) solid Colors.azure
        , color Colors.azure
        , borderBottomWidth (px 2)
        , backgroundColor Colors.white
        , height largeIconSize
        , width largeIconSize
        , fontSize (px 16)
        , property "transition" "background-color 0.2s, color 0.2s"
        , displayFlex
        , alignItems center
        , justifyContent center
        ]
        [ Attributes.class circleIconClass ]
        [ icon_
            |> Svg.withWidth circledInnerIconSize
            |> Svg.withHeight circledInnerIconSize
            |> Svg.toHtml
        ]


withoutIconCircle : Bool -> Svg.Svg -> Html msg
withoutIconCircle isFirst icon_ =
    let
        size =
            if isFirst then
                largeIconSize

            else
                iconSize
    in
    icon_
        |> Svg.withWidth size
        |> Svg.withHeight size
        |> Svg.withCss [ Css.flexShrink Css.zero ]
        |> Svg.toHtml


horizontalSpacing : Css.Px
horizontalSpacing =
    Css.px 10


circledInnerIconSize : Css.Px
circledInnerIconSize =
    Css.px 25


largeIconSize : Css.Px
largeIconSize =
    Css.px 40


iconSize : Css.Px
iconSize =
    Css.px 31


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
