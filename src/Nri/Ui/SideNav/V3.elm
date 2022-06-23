module Nri.Ui.SideNav.V3 exposing
    ( view, Config, NavAttribute
    , navLabel
    , navCss, navNotMobileCss, navMobileCss, navQuizEngineMobileCss
    , entry, entryWithChildren, html, Entry, Attribute
    , icon, custom, css, nriDescription, testId, id
    , onClick
    , href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking
    , primary, secondary
    , premiumDisplay
    )

{-|


# Changes from V2

  - change to `NavAttribute` list-based API

@docs view, Config, NavAttribute
@docs navLabel
@docs navCss, navNotMobileCss, navMobileCss, navQuizEngineMobileCss


## Entries

@docs entry, entryWithChildren, html, Entry, Attribute
@docs icon, custom, css, nriDescription, testId, id


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
import Nri.Ui
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumDisplay as PremiumDisplay exposing (PremiumDisplay)
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
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
type NavAttribute
    = NavAttribute (NavAttributeConfig -> NavAttributeConfig)


type alias NavAttributeConfig =
    { navLabel : Maybe String
    , css : List Style
    }


defaultNavAttributeConfig : NavAttributeConfig
defaultNavAttributeConfig =
    { navLabel = Nothing
    , css =
        [ flexBasis (px 250)
        , flexShrink (num 0)
        , borderRadius (px 8)
        , backgroundColor Colors.gray96
        , padding (px 20)
        , marginRight (px 20)
        ]
    }


{-| Give screenreader users context on what this particular sidenav is for.
-}
navLabel : String -> NavAttribute
navLabel str =
    NavAttribute (\config -> { config | navLabel = Just str })


{-| These styles are included automatically in the nav container:

    [ flexBasis (px 250)
    , flexShrink (num 0)
    , borderRadius (px 8)
    , backgroundColor Colors.gray96
    , padding (px 20)
    , marginRight (px 20)
    ]

-}
navCss : List Style -> NavAttribute
navCss styles =
    NavAttribute (\config -> { config | css = List.append config.css styles })


{-| -}
navNotMobileCss : List Style -> NavAttribute
navNotMobileCss styles =
    navCss [ Css.Media.withMedia [ MediaQuery.notMobile ] styles ]


{-| -}
navMobileCss : List Style -> NavAttribute
navMobileCss styles =
    navCss [ Css.Media.withMedia [ MediaQuery.mobile ] styles ]


{-| -}
navQuizEngineMobileCss : List Style -> NavAttribute
navQuizEngineMobileCss styles =
    navCss [ Css.Media.withMedia [ MediaQuery.quizEngineMobile ] styles ]


{-| -}
view : Config route msg -> List NavAttribute -> List (Entry route msg) -> Html msg
view config navAttributes entries =
    let
        appliedNavAttributes =
            List.foldl (\(NavAttribute f) b -> f b) defaultNavAttributeConfig navAttributes
    in
    styled nav
        appliedNavAttributes.css
        ([ Maybe.map Aria.label appliedNavAttributes.navLabel
         ]
            |> List.filterMap identity
        )
        (viewSkipLink config.onSkipNav
            :: List.map (viewSidebarEntry config []) entries
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
            , Css.focus
                [ outline none
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
                        :: List.map (viewSidebarEntry config [ marginLeft (px 20) ]) children
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
    in
    Nri.Ui.styled Html.Styled.a
        ("Nri-Ui-SideNav-" ++ linkFunctionName)
        (sharedEntryStyles
            ++ extraStyles
            ++ (if isCurrentRoute config entryConfig then
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
    , focus [ outline none, FocusRing.insetBoxShadow ]
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
    custom [ ExtraAttributes.nriDescription description ]


{-| -}
testId : String -> Attribute route msg
testId id_ =
    custom [ ExtraAttributes.testId id_ ]


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


setClickableAttributes :
    Maybe route
    -> (ClickableAttributes route msg -> ClickableAttributes route msg)
    -> Attribute route msg
setClickableAttributes route apply =
    Attribute
        (\attributes ->
            { attributes
                | route =
                    case route of
                        Just r ->
                            Just r

                        Nothing ->
                            attributes.route
                , clickableAttributes = apply attributes.clickableAttributes
            }
        )


{-| -}
onClick : msg -> Attribute route msg
onClick msg =
    setClickableAttributes Nothing (ClickableAttributes.onClick msg)


{-| -}
href : route -> Attribute route msg
href route =
    setClickableAttributes (Just route) (ClickableAttributes.href route)


{-| Use this link for routing within a single page app.

This will make a normal <a> tag, but change the Events.onClick behavior to avoid reloading the page.

See <https://github.com/elm-lang/html/issues/110> for details on this implementation.

-}
linkSpa : route -> Attribute route msg
linkSpa route =
    setClickableAttributes (Just route)
        (ClickableAttributes.linkSpa route)


{-| -}
linkWithMethod : { method : String, url : route } -> Attribute route msg
linkWithMethod config =
    setClickableAttributes (Just config.url)
        (ClickableAttributes.linkWithMethod config)


{-| -}
linkWithTracking : { track : msg, url : route } -> Attribute route msg
linkWithTracking config =
    setClickableAttributes (Just config.url)
        (ClickableAttributes.linkWithTracking config)


{-| -}
linkExternal : String -> Attribute route msg
linkExternal url =
    setClickableAttributes Nothing (ClickableAttributes.linkExternal url)


{-| -}
linkExternalWithTracking : { track : msg, url : String } -> Attribute route msg
linkExternalWithTracking config =
    setClickableAttributes Nothing (ClickableAttributes.linkExternalWithTracking config)
