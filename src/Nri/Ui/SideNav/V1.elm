module Nri.Ui.SideNav.V1 exposing (view, Config)

{-|

@docs view, Config

-}

import Accessibility.Styled exposing (..)
import Css exposing (..)
import Css.Media as Media
import Html.Styled
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon
import String exposing (toLower)
import String.Extra exposing (dasherize)


{-| Use `link` or `externalLink` to create a sidebar link.
-}
type SidebarEntry route msg
    = Link (LinkConfig route msg)
    | ExternalLink ExternalLinkConfig


{-| -}
type alias LinkConfig route msg =
    { title : String
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
type alias ExternalLinkConfig =
    { icon : Svg
    , plaintext : String
    , url : String
    }


{-| -}
externalLink : ExternalLinkConfig -> SidebarEntry route msg
externalLink =
    ExternalLink


{-| -}
type alias Config route =
    { userPremiumLevel : PremiumLevel
    , isCurrentRoute : route -> Bool
    }


{-| -}
view : Config route -> List (SidebarEntry route msg) -> Html msg
view config entries =
    styled nav
        [ width (px 300)
        , borderRadius (px 8)
        , backgroundColor Colors.gray96
        , padding (px 20)
        , marginRight (px 20)
        , position sticky
        , property "position" "-webkit-sticky"
        , top (px 20)
        , overflowY auto
        , minWidth (px 250)
        , Media.withMedia
            [ Media.all [ Media.maxWidth (px 600) ] ]
            -- custom, smaller breakpoint since the sidebar nav allows access to subcategories
            -- in the assignment library
            [ width (pct 100)
            , margin2 (px 20) zero
            ]
        ]
        []
        (List.map (viewSidebarEntry config []) entries)


viewSidebarEntry : Config route -> List Css.Style -> SidebarEntry route msg -> Html msg
viewSidebarEntry config extraStyles sidebarEntry =
    case sidebarEntry of
        ExternalLink { plaintext, icon, url } ->
            styled a
                (sharedEntryStyles
                    ++ extraStyles
                    ++ [ backgroundColor Colors.white
                       , boxShadow3 zero (px 2) Colors.gray75
                       , border3 (px 1) solid Colors.gray75
                       , marginBottom (px 10)
                       ]
                )
                [ Attributes.href url ]
                [ icon
                    |> Svg.withWidth (px 20)
                    |> Svg.withHeight (px 20)
                    |> Svg.withCss [ marginRight (px 5) ]
                    |> Svg.toHtml
                , text plaintext
                ]

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
                    viewSidebarLeaf config entry

            else
                viewLockedEntry entry.title


anyLinkDescendants : (LinkConfig route msg -> Bool) -> LinkConfig route msg -> Bool
anyLinkDescendants f { children } =
    List.any
        (\child ->
            case child of
                Link entry ->
                    f entry || anyLinkDescendants f entry

                ExternalLink _ ->
                    False
        )
        children


viewSidebarLeaf :
    Config route
    -> LinkConfig route msg
    -> Html msg
viewSidebarLeaf config { title, route, attributes } =
    styled Html.Styled.a
        (sharedEntryStyles
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
        [ text title ]


viewLockedEntry : String -> Html msg
viewLockedEntry title =
    let
        lockedEntryId =
            -- TODO: pass in ids
            "browse-and-assign-locked-entry__" ++ dasherize (toLower title)
    in
    styled Html.Styled.button
        [ paddingLeft (px 20)
        , paddingRight (px 20)
        , displayFlex
        , alignItems center
        , Fonts.baseFont
        , fontSize (px 15)
        , textDecoration none
        , color Colors.gray45
        , height (px 45)
        , borderStyle none
        , backgroundColor Colors.gray96
        , textAlign left
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


sharedEntryStyles : List Css.Style
sharedEntryStyles =
    [ paddingLeft (px 20)
    , paddingRight (px 20)
    , height (px 45)
    , displayFlex
    , borderRadius (px 8)
    , alignItems center
    , Fonts.baseFont
    , color Colors.navy
    , textDecoration none
    , fontSize (px 15)
    , fontWeight (int 600)
    ]
