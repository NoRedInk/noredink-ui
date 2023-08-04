module Nri.Ui.SideNav.V5 exposing
    ( view, Config, NavAttribute
    , collapsible
    , navLabel, navId
    , navCss, navNotMobileCss, navMobileCss, navQuizEngineMobileCss
    , entry, entryWithChildren, html, Entry, Attribute
    , icon, rightIcon
    , custom, css, nriDescription, testId, id
    , onClick
    , href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking
    , primary, secondary
    , premiumDisplay
    )

{-|


### Changes from V4


## View

@docs view, Config, NavAttribute
@docs collapsible
@docs navLabel, navId
@docs navCss, navNotMobileCss, navMobileCss, navQuizEngineMobileCss


## Entries

@docs entry, entryWithChildren, html, Entry, Attribute
@docs icon, rightIcon
@docs custom, css, nriDescription, testId, id


## Behavior

@docs onClick
@docs href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking


## Change the color scheme

@docs primary, secondary


## Change the state

@docs premiumDisplay

-}

import Accessibility.Styled exposing (..)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Style as Style
import ClickableAttributes exposing (ClickableAttributes)
import Css exposing (..)
import Css.Media
import Html.Styled
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Maybe.Extra
import Nri.Ui
import Nri.Ui.AnimatedIcon.V1 as AnimatedIcon
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumDisplay as PremiumDisplay exposing (PremiumDisplay)
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Tooltip.V3 as Tooltip
import Nri.Ui.UiIcon.V1 as UiIcon


{-| Use `entry` to create a sidebar entry.
-}
type Entry route msg
    = Entry (List (Entry route msg)) (EntryConfig route msg)
    | Html (List (Html msg))


{-| -}
entry : String -> List (Attribute route msg) -> Entry route msg
entry title attributes =
    attributes
        |> List.foldl (\(Attribute attribute) b -> attribute b) (build title)
        |> Entry []


{-| -}
entryWithChildren : String -> List (Attribute route msg) -> List (Entry route msg) -> Entry route msg
entryWithChildren title attributes children =
    attributes
        |> List.foldl (\(Attribute attribute) b -> attribute b) (build title)
        |> Entry children


{-| -}
html : List (Html msg) -> Entry route msg
html =
    Html


{-| -}
type alias Config route msg =
    { isCurrentRoute : route -> Bool
    , routeToString : route -> String
    , onSkipNav : msg
    }


{-| -}
type NavAttribute msg
    = NavAttribute (NavAttributeConfig msg -> NavAttributeConfig msg)


type alias NavAttributeConfig msg =
    { navLabel : Maybe String
    , navId : Maybe String
    , css : List Style
    , collapsible : Maybe (CollapsibleConfig msg)
    }


defaultNavAttributeConfig : NavAttributeConfig msg
defaultNavAttributeConfig =
    { navLabel = Nothing
    , navId = Nothing
    , css = []
    , collapsible = Nothing
    }


{-| Give screenreader users context on what this particular sidenav is for.

If the nav is collapsible, this value will also be used for the sidenav tooltips.

-}
navLabel : String -> NavAttribute msg
navLabel str =
    NavAttribute (\config -> { config | navLabel = Just str })


{-| -}
navId : String -> NavAttribute msg
navId str =
    NavAttribute (\config -> { config | navId = Just str })


{-| These styles are included automatically in the nav container:

    [ flexBasis (px 300)
    , flexShrink (num 0)
    , borderRadius (px 8)
    , backgroundColor Colors.gray96
    , padding (px 20)
    , marginRight (px 20)
    ]

-}
navCss : List Style -> NavAttribute msg
navCss styles =
    NavAttribute (\config -> { config | css = List.append config.css styles })


{-| -}
navNotMobileCss : List Style -> NavAttribute msg
navNotMobileCss styles =
    navCss [ Css.Media.withMedia [ MediaQuery.notMobile ] styles ]


{-| -}
navMobileCss : List Style -> NavAttribute msg
navMobileCss styles =
    navCss [ Css.Media.withMedia [ MediaQuery.mobile ] styles ]


{-| -}
navQuizEngineMobileCss : List Style -> NavAttribute msg
navQuizEngineMobileCss styles =
    navCss [ Css.Media.withMedia [ MediaQuery.quizEngineMobile ] styles ]


{-| -}
type alias CollapsibleConfig msg =
    { isOpen : Bool
    , toggle : Bool -> msg
    , isTooltipOpen : Bool
    , toggleTooltip : Bool -> msg
    }


{-| -}
collapsible : CollapsibleConfig msg -> NavAttribute msg
collapsible collapsible_ =
    NavAttribute (\config -> { config | collapsible = Just collapsible_ })


{-| -}
view : Config route msg -> List (NavAttribute msg) -> List (Entry route msg) -> Html msg
view config navAttributes entries =
    let
        appliedNavAttributes =
            List.foldl (\(NavAttribute f) b -> f b) defaultNavAttributeConfig navAttributes

        showNav =
            Maybe.map .isOpen appliedNavAttributes.collapsible
                |> Maybe.withDefault True

        sidenavId =
            Maybe.withDefault defaultSideNavId appliedNavAttributes.navId

        defaultCss =
            [ if showNav then
                case appliedNavAttributes.collapsible of
                    Just _ ->
                        Css.batch
                            [ Css.flexBasis (Css.px 295)
                            , Css.padding4 (Css.px 25) (Css.px 25) (Css.px 20) (Css.px 20)
                            ]

                    Nothing ->
                        Css.batch
                            [ Css.flexBasis (Css.px 300)
                            , Css.padding (Css.px 20)
                            ]

              else
                Css.flexBasis (Css.px 5)
            , flexShrink (num 0)
            , marginRight (px 20)
            , position relative
            , borderRadius (px 8)
            , backgroundColor Colors.gray96
            , alignSelf flexStart
            , Css.Media.withMedia [ MediaQuery.mobile ]
                [ Css.property "flex-basis" "unset"
                , marginRight Css.zero
                , marginBottom (Css.px 20)
                , width (pct 100)
                , case Maybe.map .isOpen appliedNavAttributes.collapsible of
                    Just _ ->
                        Css.padding (Css.px 10)

                    Nothing ->
                        Css.batch []
                ]
            ]
    in
    div [ Attributes.css (defaultCss ++ appliedNavAttributes.css) ]
        [ viewSkipLink config.onSkipNav
        , case entries of
            [] ->
                text ""

            _ ->
                viewNav sidenavId config appliedNavAttributes entries showNav
        ]


defaultSideNavId : String
defaultSideNavId =
    "sidenav"


viewOpenCloseButton : String -> Maybe String -> Maybe String -> CollapsibleConfig msg -> Html msg
viewOpenCloseButton sidenavId navLabel_ currentEntry { isOpen, toggle, isTooltipOpen, toggleTooltip } =
    let
        name =
            Maybe.withDefault "sidebar" navLabel_

        ( action, icon_ ) =
            if isOpen then
                ( "Close " ++ name
                , UiIcon.openClose
                )

            else
                ( "Open " ++ name
                , UiIcon.openClose
                    |> Svg.withCss [ Css.transform (rotate (deg 180)) ]
                )

        trigger attributes =
            ClickableSvg.button action
                icon_
                ([ ClickableSvg.custom
                    [ Aria.controls [ sidenavId ]
                    , Aria.expanded isOpen
                    ]
                 , ClickableSvg.onClick (toggle (not isOpen))
                 , ClickableSvg.secondary
                 , ClickableSvg.withBorder
                 , ClickableSvg.iconForMobile (AnimatedIcon.mobileOpenClose isOpen)
                 ]
                    ++ attributes
                )

        nonMobileTooltipView =
            Tooltip.view
                { trigger = \tooltipAttributes -> trigger [ ClickableSvg.custom tooltipAttributes ]
                , id = "open-close-sidebar-tooltip"
                }
                [ Tooltip.open isTooltipOpen
                , Tooltip.onToggle toggleTooltip
                , Tooltip.plaintext action
                , Tooltip.smallPadding
                , Tooltip.fitToContent
                , if isOpen then
                    Tooltip.onLeft

                  else
                    Tooltip.onRight
                , Tooltip.containerCss
                    [ -- Hide the tooltip for mobile. We'll display static text instead
                      Css.Media.withMedia [ MediaQuery.mobile ]
                        [ Css.display Css.none ]
                    ]
                , Tooltip.containerCss
                    (if isOpen then
                        [ Css.Media.withMedia [ MediaQuery.notMobile ]
                            [ Css.position Css.absolute
                            , Css.top (Css.px 10)
                            , Css.right (Css.px 10)
                            ]
                        ]

                     else
                        []
                    )
                ]

        mobileButtonView =
            div
                [ Attributes.css
                    [ -- Hide the plain button/static text if not on the mobile view
                      Css.display Css.none
                    , Css.Media.withMedia [ MediaQuery.mobile ]
                        [ Css.displayFlex ]
                    ]
                ]
                [ trigger []
                , viewJust mobileCurrentPage currentEntry
                ]
    in
    div []
        [ nonMobileTooltipView
        , mobileButtonView
        ]


mobileCurrentPage : String -> Html msg
mobileCurrentPage name =
    span
        [ AttributesExtra.nriDescription "mobile-current-page-name"
        , Attributes.css (sharedEntryStyles ++ [ Css.display Css.inline, Css.padding (Css.px 8) ])
        ]
        [ text name ]


viewNav : String -> Config route msg -> NavAttributeConfig msg -> List (Entry route msg) -> Bool -> Html msg
viewNav sidenavId config appliedNavAttributes entries showNav =
    let
        currentEntry =
            currentRouteName config.isCurrentRoute entries

        entryStyles =
            if showNav then
                []

            else
                [ Css.display Css.none ]
    in
    nav
        ([ Maybe.map Aria.label appliedNavAttributes.navLabel
         , Just (Attributes.id sidenavId)
         ]
            |> List.filterMap identity
        )
        (viewJust (viewOpenCloseButton sidenavId appliedNavAttributes.navLabel currentEntry) appliedNavAttributes.collapsible
            :: List.map (viewSidebarEntry config entryStyles) entries
        )


viewSkipLink : msg -> Html msg
viewSkipLink onSkip =
    ClickableText.button "Skip to main content"
        [ ClickableText.icon UiIcon.arrowPointingRight
        , ClickableText.small
        , ClickableText.custom [ Attributes.class FocusRing.customClass ]
        , ClickableText.css
            [ Css.pseudoClass "not(:focus)"
                [ Style.invisibleStyle
                ]
            , Css.pseudoClass "focus-visible"
                [ Css.outline3 (Css.px 2) Css.solid Css.transparent
                , FocusRing.outerBoxShadow
                ]
            , Css.padding (Css.px 2)
            , Css.borderRadius (Css.px 4)
            ]
        , ClickableText.onClick onSkip
        ]


viewSidebarEntry : Config route msg -> List Css.Style -> Entry route msg -> Html msg
viewSidebarEntry config extraStyles entry_ =
    case entry_ of
        Entry children entryConfig ->
            if entryConfig.premiumDisplay == PremiumDisplay.PremiumLocked then
                viewLockedEntry extraStyles entryConfig

            else if anyLinkDescendants (isCurrentRoute config) children then
                div [ Attributes.css extraStyles ]
                    (styled span
                        (sharedEntryStyles
                            ++ [ backgroundColor Colors.gray92
                               , color Colors.navy
                               , fontWeight bold
                               , cursor default
                               , marginBottom (px 10)
                               ]
                        )
                        []
                        [ text entryConfig.title ]
                        :: List.map
                            (viewSidebarEntry config (marginLeft (px 20) :: extraStyles))
                            children
                    )

            else
                viewSidebarLeaf config extraStyles entryConfig

        Html html_ ->
            div [ Attributes.css extraStyles ] html_


isCurrentRoute : Config route msg -> EntryConfig route msg -> Bool
isCurrentRoute config { route } =
    Maybe.map config.isCurrentRoute route
        |> Maybe.withDefault False


anyLinkDescendants : (EntryConfig route msg -> Bool) -> List (Entry route msg) -> Bool
anyLinkDescendants f children =
    List.any
        (\entry_ ->
            case entry_ of
                Entry children_ entryConfig ->
                    f entryConfig || anyLinkDescendants f children_

                Html _ ->
                    False
        )
        children


currentRouteName : (route -> Bool) -> List (Entry route msg) -> Maybe String
currentRouteName isCurrentRoute_ entries =
    List.foldl
        (\entry_ acc ->
            Maybe.Extra.or acc
                (case entry_ of
                    Entry children_ entryConfig ->
                        case Maybe.map isCurrentRoute_ entryConfig.route of
                            Just True ->
                                Just entryConfig.title

                            _ ->
                                currentRouteName isCurrentRoute_ children_

                    Html _ ->
                        acc
                )
        )
        Nothing
        entries


viewSidebarLeaf :
    Config route msg
    -> List Style
    -> EntryConfig route msg
    -> Html msg
viewSidebarLeaf config extraStyles entryConfig =
    let
        ( linkFunctionName, attributes ) =
            ClickableAttributes.toLinkAttributes
                { routeToString = config.routeToString
                , isDisabled = False
                }
                entryConfig.clickableAttributes

        isCurrent =
            isCurrentRoute config entryConfig
    in
    Nri.Ui.styled Html.Styled.a
        ("Nri-Ui-SideNav-" ++ linkFunctionName)
        (sharedEntryStyles
            ++ extraStyles
            ++ (if isCurrent then
                    [ backgroundColor Colors.glacier
                    , color Colors.navy
                    , fontWeight bold
                    , visited [ color Colors.navy ]
                    ]

                else
                    []
               )
            ++ entryConfig.customStyles
        )
        (Attributes.class FocusRing.customClass
            :: AttributesExtra.includeIf isCurrent Aria.currentPage
            :: attributes
            ++ entryConfig.customAttributes
        )
        [ viewJust
            (\icon_ ->
                icon_
                    |> Svg.withWidth (px 20)
                    |> Svg.withHeight (px 20)
                    |> Svg.withCss [ marginRight (px 5) ]
                    |> Svg.toHtml
            )
            entryConfig.icon
        , text entryConfig.title
        , viewJust
            (\icon_ ->
                icon_
                    |> Svg.withWidth (px 20)
                    |> Svg.withHeight (px 20)
                    |> Svg.withCss [ marginLeft (px 5) ]
                    |> Svg.toHtml
            )
            entryConfig.rightIcon
        ]


viewLockedEntry : List Style -> EntryConfig route msg -> Html msg
viewLockedEntry extraStyles entryConfig =
    styled Html.Styled.button
        [ batch sharedEntryStyles
        , important (color Colors.gray45)
        , borderWidth zero
        , batch extraStyles
        ]
        (case entryConfig.onLockedContent of
            Just event ->
                Events.onClick event
                    :: Attributes.class FocusRing.customClass
                    :: entryConfig.customAttributes

            Nothing ->
                entryConfig.customAttributes
        )
        [ UiIcon.premiumLock
            |> Svg.withWidth (px 17)
            |> Svg.withHeight (px 25)
            |> Svg.withCss [ marginRight (px 10), minWidth (px 17) ]
            |> Svg.toHtml
        , text entryConfig.title
        ]


sharedEntryStyles : List Style
sharedEntryStyles =
    [ padding2 (px 13) (px 20)
    , Css.pseudoClass "focus-visible"
        [ Css.outline3 (Css.px 2) Css.solid Css.transparent
        , FocusRing.insetBoxShadow
        ]
    , Css.property "word-break" "normal"
    , Css.property "overflow-wrap" "anywhere"
    , displayFlex
    , borderRadius (px 8)
    , alignItems center
    , Fonts.baseFont
    , color Colors.navy
    , backgroundColor transparent
    , textDecoration none
    , fontSize (px 15)
    , fontWeight (int 600)
    , textAlign left
    , cursor pointer
    ]



-- Entry Customization helpers


{-| -}
type alias EntryConfig route msg =
    { icon : Maybe Svg
    , rightIcon : Maybe Svg
    , title : String
    , route : Maybe route
    , clickableAttributes : ClickableAttributes route msg
    , customAttributes : List (Html.Styled.Attribute msg)
    , customStyles : List Style
    , premiumDisplay : PremiumDisplay
    , onLockedContent : Maybe msg
    }


build : String -> EntryConfig route msg
build title =
    { icon = Nothing
    , rightIcon = Nothing
    , title = title
    , route = Nothing
    , clickableAttributes = ClickableAttributes.init
    , customAttributes = []
    , customStyles = []
    , premiumDisplay = PremiumDisplay.Free
    , onLockedContent = Nothing
    }


{-| -}
type Attribute route msg
    = Attribute (EntryConfig route msg -> EntryConfig route msg)


{-| -}
icon : Svg -> Attribute route msg
icon icon_ =
    Attribute (\attributes -> { attributes | icon = Just icon_ })


{-| -}
rightIcon : Svg -> Attribute route msg
rightIcon icon_ =
    Attribute (\attributes -> { attributes | rightIcon = Just icon_ })


{-| -}
premiumDisplay : PremiumDisplay -> msg -> Attribute route msg
premiumDisplay display ifLocked =
    Attribute
        (\attributes ->
            { attributes
                | premiumDisplay = display
                , onLockedContent = Just ifLocked
            }
        )


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying Button styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Styled.Attribute msg) -> Attribute route msg
custom attributes =
    Attribute
        (\config ->
            { config
                | customAttributes = List.append config.customAttributes attributes
            }
        )


{-| -}
nriDescription : String -> Attribute route msg
nriDescription description =
    custom [ AttributesExtra.nriDescription description ]


{-| -}
testId : String -> Attribute route msg
testId id_ =
    custom [ AttributesExtra.testId id_ ]


{-| -}
id : String -> Attribute route msg
id id_ =
    custom [ Attributes.id id_ ]


{-| -}
css : List Style -> Attribute route msg
css styles =
    Attribute
        (\config ->
            { config
                | customStyles = List.append config.customStyles styles
            }
        )


{-| -}
primary : Attribute route msg
primary =
    Attribute (\attributes -> { attributes | customStyles = [] })


{-| -}
secondary : Attribute route msg
secondary =
    Attribute
        (\attributes ->
            { attributes
                | customStyles =
                    [ backgroundColor Colors.white
                    , boxShadow3 zero (px 2) Colors.gray75
                    , border3 (px 1) solid Colors.gray75
                    ]
            }
        )



-- LINKING, CLICKING, and TRACKING BEHAVIOR


setClickableAttributesWithRoute : route -> (EntryConfig route msg -> EntryConfig route msg) -> Attribute route msg
setClickableAttributesWithRoute route apply =
    Attribute (\attributes -> apply { attributes | route = Just route })


{-| -}
onClick : msg -> Attribute route msg
onClick msg =
    Attribute (ClickableAttributes.onClick msg)


{-| -}
href : route -> Attribute route msg
href route =
    setClickableAttributesWithRoute route (ClickableAttributes.href route)


{-| Use this link for routing within a single page app.

This will make a normal <a> tag, but change the Events.onClick behavior to avoid reloading the page.

See <https://github.com/elm-lang/html/issues/110> for details on this implementation.

-}
linkSpa : route -> Attribute route msg
linkSpa route =
    setClickableAttributesWithRoute route
        (ClickableAttributes.linkSpa route)


{-| -}
linkWithMethod : { method : String, url : route } -> Attribute route msg
linkWithMethod config =
    setClickableAttributesWithRoute config.url
        (ClickableAttributes.linkWithMethod config)


{-| -}
linkWithTracking : { track : msg, url : route } -> Attribute route msg
linkWithTracking config =
    setClickableAttributesWithRoute config.url
        (ClickableAttributes.linkWithTracking config)


{-| -}
linkExternal : String -> Attribute route msg
linkExternal url =
    Attribute (ClickableAttributes.linkExternal url)


{-| -}
linkExternalWithTracking : { track : msg, url : String } -> Attribute route msg
linkExternalWithTracking config =
    Attribute (ClickableAttributes.linkExternalWithTracking config)
