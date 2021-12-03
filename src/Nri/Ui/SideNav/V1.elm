module Nri.Ui.SideNav.V1 exposing
    ( view, Config
    , entry, Entry
    , icon, custom, css, nriDescription, testId, id
    , onClick
    , href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking
    , primary, secondary
    , premiumLevel
    )

{-|

@docs view, Config
@docs entry, Entry
@docs icon, custom, css, nriDescription, testId, id


## Behavior

@docs onClick
@docs href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking


## Change the color scheme

@docs primary, secondary


## Change the state

@docs premiumLevel

-}

import Accessibility.Styled exposing (..)
import ClickableAttributes exposing (ClickableAttributes)
import Css exposing (..)
import Css.Media as Media
import Html.Styled
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Nri.Ui
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Data.PremiumLevel as PremiumLevel exposing (PremiumLevel)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.Html.V3 exposing (viewJust)
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon
import String exposing (toLower)
import String.Extra exposing (dasherize)


{-| Use `entry` to create a sidebar entry.
-}
type Entry route msg
    = Entry (EntryConfig route msg)


{-| -}
entry : String -> route -> List (Attribute route msg) -> Entry route msg
entry title route attributes =
    attributes
        |> List.foldl (\(Attribute attribute) b -> attribute b) (build title route)
        |> Entry


{-| -}
type alias Config route msg =
    { userPremiumLevel : PremiumLevel
    , isCurrentRoute : route -> Bool
    , onSkipNav : msg
    , css : List Style
    }


{-| -}
view : Config route msg -> List (Entry route msg) -> Html msg
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


viewSidebarEntry : Config route msg -> List Css.Style -> Entry route msg -> Html msg
viewSidebarEntry config extraStyles (Entry entry_) =
    if PremiumLevel.allowedFor entry_.premiumLevel config.userPremiumLevel then
        if anyLinkDescendants (.route >> config.isCurrentRoute) entry_ then
            div [ Attributes.css extraStyles ]
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
                    [ text entry_.title ]
                    :: List.map (viewSidebarEntry config [ marginLeft (px 20) ])
                        entry_.children
                )

        else
            viewSidebarLeaf config extraStyles entry_

    else
        viewLockedEntry entry_.title extraStyles


anyLinkDescendants : (EntryConfig route msg -> Bool) -> EntryConfig route msg -> Bool
anyLinkDescendants f { children } =
    List.any (\(Entry entry_) -> f entry_ || anyLinkDescendants f entry_) children


viewSidebarLeaf :
    Config route msg
    -> List Style
    -> EntryConfig route msg
    -> Html msg
viewSidebarLeaf config extraStyles entryConfig =
    let
        ( linkFunctionName, attributes ) =
            ClickableAttributes.toLinkAttributes entryConfig.clickableAttributes
    in
    Nri.Ui.styled Html.Styled.a
        ("Nri-Ui-SideNav-" ++ linkFunctionName)
        (sharedEntryStyles
            ++ extraStyles
            ++ (if config.isCurrentRoute entryConfig.route then
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
        (attributes ++ entryConfig.customAttributes)
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



-- Entry Customization helpers


{-| -}
type alias EntryConfig route msg =
    { icon : Maybe Svg
    , title : String
    , route : route
    , clickableAttributes : ClickableAttributes msg
    , customAttributes : List (Html.Styled.Attribute msg)
    , customStyles : List Style
    , children : List (Entry route msg)
    , premiumLevel : PremiumLevel
    }


build : String -> route -> EntryConfig route msg
build title route =
    { icon = Nothing
    , title = title
    , route = route
    , clickableAttributes = ClickableAttributes.init
    , customAttributes = []
    , customStyles = []
    , children = []
    , premiumLevel = PremiumLevel.Free
    }


type Attribute route msg
    = Attribute (EntryConfig route msg -> EntryConfig route msg)


{-| -}
icon : Svg -> Attribute route msg
icon icon_ =
    Attribute (\attributes -> { attributes | icon = Just icon_ })


{-| -}
premiumLevel : PremiumLevel -> msg -> Attribute route msg
premiumLevel level ifLocked =
    -- TODO: adds the lock click behavior
    Attribute (\attributes -> { attributes | premiumLevel = level })


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
    (ClickableAttributes msg -> ClickableAttributes msg)
    -> Attribute route msg
setClickableAttributes apply =
    Attribute
        (\attributes ->
            { attributes | clickableAttributes = apply attributes.clickableAttributes }
        )


{-| -}
onClick : msg -> Attribute route msg
onClick msg =
    setClickableAttributes (ClickableAttributes.onClick msg)


{-| -}
href : String -> Attribute route msg
href url =
    setClickableAttributes (ClickableAttributes.href url)


{-| Use this link for routing within a single page app.

This will make a normal <a> tag, but change the Events.onClick behavior to avoid reloading the page.

See <https://github.com/elm-lang/html/issues/110> for details on this implementation.

-}
linkSpa : String -> Attribute route msg
linkSpa url =
    setClickableAttributes (ClickableAttributes.linkSpa url)


{-| -}
linkWithMethod : { method : String, url : String } -> Attribute route msg
linkWithMethod config =
    setClickableAttributes (ClickableAttributes.linkWithMethod config)


{-| -}
linkWithTracking : { track : msg, url : String } -> Attribute route msg
linkWithTracking config =
    setClickableAttributes (ClickableAttributes.linkWithTracking config)


{-| -}
linkExternal : String -> Attribute route msg
linkExternal url =
    setClickableAttributes (ClickableAttributes.linkExternal url)


{-| -}
linkExternalWithTracking : { track : msg, url : String } -> Attribute route msg
linkExternalWithTracking config =
    setClickableAttributes (ClickableAttributes.linkExternalWithTracking config)
