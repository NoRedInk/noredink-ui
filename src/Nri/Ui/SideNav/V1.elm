module Nri.Ui.SideNav.V1 exposing
    ( view, Config, SidebarEntry
    , link, LinkConfig
    , withBorderStyles
    )

{-|

@docs view, Config, SidebarEntry
@docs link, LinkConfig

-}

import Accessibility.Styled exposing (..)
import Css exposing (..)
import Css.Media as Media
import Html.Styled
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon
import String exposing (toLower)
import String.Extra exposing (dasherize)


{-| Use `link` to create a sidebar link.
-}
type SidebarEntry route msg
    = Link (LinkConfig route msg)


{-| -}
type alias LinkConfig route msg =
    { icon : Maybe Svg
    , title : String
    , route : route
    , attributes : List (Html.Styled.Attribute msg)
    , children : List (SidebarEntry route msg)
    , premiumLevel : PremiumLevel
    }


{-| -}
link : LinkConfig route msg -> SidebarEntry route msg
link =
    Link


{-| -}
type alias Config route msg =
    { userPremiumLevel : PremiumLevel
    , isCurrentRoute : route -> Bool
    , onSkipNav : msg
    , css : List Style
    }


{-| -}
view : Config route msg -> List (SidebarEntry route msg) -> Html msg
view config entries =
    styled nav
        [ flexBasis (px 250)
        , flexShrink (num 0)
        , borderRadius (px 8)
        , backgroundColor Colors.gray96
        , padding (px 20)
        , marginRight (px 20)
        , batch config.css
        ]
        []
        (viewSkipLink config.onSkipNav
            :: List.map (viewSidebarEntry config []) entries
        )


viewSkipLink : msg -> Html msg
viewSkipLink onSkip =
    ClickableText.button "Skip to main content"
        [ ClickableText.icon UiIcon.arrowPointingRight
        , ClickableText.small
        , ClickableText.css
            [ Css.pseudoClass "not(:focus)"
                -- TODO: use Accessibility.Styled.Style.invisibleStyle
                -- when we're on a higher version of tesk9/accessible-html-with-css
                -- than 2.2.1
                [ Css.property "clip" "rect(1px, 1px, 1px, 1px)"
                , Css.position Css.absolute
                , Css.height (Css.px 1)
                , Css.width (Css.px 1)
                , Css.overflow Css.hidden
                , Css.margin (Css.px -1)
                , Css.padding Css.zero
                , Css.border Css.zero
                ]
            ]
        , ClickableText.onClick onSkip
        ]


viewSidebarEntry : Config route msg -> List Css.Style -> SidebarEntry route msg -> Html msg
viewSidebarEntry config extraStyles sidebarEntry =
    case sidebarEntry of
        Link entry ->
            if PremiumLevel.allowedFor entry.premiumLevel config.userPremiumLevel then
                if anyLinkDescendants (.route >> config.isCurrentRoute) entry then
                    div [ css extraStyles ]
                        (styled span
                            (sharedEntryStyles
                                ++ [ backgroundColor Colors.gray92
                                   , marginBottom (px 10)
                                   , color Colors.navy
                                   , fontWeight bold
                                   , cursor default
                                   ]
                            )
                            []
                            [ text entry.title ]
                            :: List.map (viewSidebarEntry config [ marginLeft (px 20) ])
                                entry.children
                        )

                else
                    viewSidebarLeaf config extraStyles entry

            else
                viewLockedEntry entry.title extraStyles


anyLinkDescendants : (LinkConfig route msg -> Bool) -> LinkConfig route msg -> Bool
anyLinkDescendants f { children } =
    List.any (\(Link entry) -> f entry || anyLinkDescendants f entry) children


viewSidebarLeaf :
    Config route msg
    -> List Style
    -> LinkConfig route msg
    -> Html msg
viewSidebarLeaf config extraStyles { icon, title, route, attributes } =
    styled Html.Styled.a
        (sharedEntryStyles
            ++ extraStyles
            ++ (if config.isCurrentRoute route then
                    [ backgroundColor Colors.glacier
                    , color Colors.navy
                    , fontWeight bold
                    , visited [ color Colors.navy ]
                    ]

                else
                    []
               )
        )
        attributes
        [ viewJust
            (\icon_ ->
                icon_
                    |> Svg.withWidth (px 20)
                    |> Svg.withHeight (px 20)
                    |> Svg.withCss [ marginRight (px 5) ]
                    |> Svg.toHtml
            )
            icon
        , text title
        ]


viewLockedEntry : String -> List Style -> Html msg
viewLockedEntry title extraStyles =
    let
        lockedEntryId =
            -- TODO: pass in ids
            "browse-and-assign-locked-entry__" ++ dasherize (toLower title)
    in
    styled Html.Styled.button
        [ batch sharedEntryStyles
        , important (color Colors.gray45)
        , borderWidth zero
        , batch extraStyles
        ]
        [ -- TODO: reimplement lock click behavior!
          --Events.onClick (launchPremiumModal lockedEntryId) ,
          Attributes.id lockedEntryId
        ]
        [ UiIcon.premiumLock
            |> Svg.withWidth (px 17)
            |> Svg.withHeight (px 25)
            |> Svg.withCss [ marginRight (px 10), minWidth (px 17) ]
            |> Svg.toHtml
        , text title
        ]


sharedEntryStyles : List Style
sharedEntryStyles =
    [ paddingLeft (px 20)
    , paddingRight (px 20)
    , height (px 45)
    , displayFlex
    , borderRadius (px 8)
    , alignItems center
    , Fonts.baseFont
    , color Colors.navy
    , backgroundColor transparent
    , textDecoration none
    , fontSize (px 15)
    , fontWeight (int 600)
    , marginBottom (px 10)
    ]


withBorderStyles : List Style
withBorderStyles =
    -- TODO: add a convenient way to use these styels
    [ backgroundColor Colors.white
    , boxShadow3 zero (px 2) Colors.gray75
    , border3 (px 1) solid Colors.gray75
    ]
