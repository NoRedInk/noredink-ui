module Nri.Ui.ClickableText.V4 exposing
    ( button
    , link
    , Attribute
    , small, medium, large, modal
    , appearsInline
    , onClick, submit, opensModal
    , href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking
    , disabled
    , icon, rightIcon
    , hideIconForMobile, hideIconFor
    , custom, nriDescription, testId, id
    , hideTextForMobile, hideTextFor
    , css, notMobileCss, mobileCss, quizEngineMobileCss, rightIconCss
    )

{-| Patch changes

  - switchs `Medium` size to the default


# Changes from V3

  - removes the `caption` attribute
  - removes `Css.display Css.inlineBlock`
  - inherits font family and weight from parent
  - adjusts font sizes
  - uses `Css.display Css.inlineFlex` only on buttons
  - removes `"-v2"` from data descriptor
  - uses dataDescriptor for `"clickable-text-label"`
  - adds `Inherited` size as default
  - adjusts icon margins for `Inherited` size


# About:

ClickableText looks different from Nri.Ui.Button in that it displays without margin or padding.
ClickableText has the suave, traditional look of a "link"!

For accessibility purposes, buttons that perform an action on the current page should be HTML `<button>`
elements and are created here with `*Button` functions. Buttons that take the user to a new page should be
HTML `<a>` elements and are created here with `*Link` functions.


# `<button>` creators

@docs button


# `<a>` creators

@docs link


# Attributes

@docs Attribute


## Sizing

@docs small, medium, large, modal


## Appearance

@docs appearsInline


## Behavior

@docs onClick, submit, opensModal
@docs href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking
@docs disabled


## Icons

@docs icon, rightIcon
@docs hideIconForMobile, hideIconFor


## Customization

@docs custom, nriDescription, testId, id


### CSS

@docs hideTextForMobile, hideTextFor
@docs css, notMobileCss, mobileCss, quizEngineMobileCss, rightIconCss

-}

import Accessibility.Styled.Style exposing (invisibleStyle)
import ClickableAttributes exposing (ClickableAttributes)
import Css exposing (Style)
import Css.Global
import Css.Media exposing (MediaQuery)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes
import Nri.Ui
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1 as Svg exposing (Svg)


label : String -> Attribute msg
label label_ =
    set (\attributes -> { attributes | label = label_ })


{-| -}
small : Attribute msg
small =
    set (\attributes -> { attributes | size = Small })


{-| `Medium` is the default size.
-}
medium : Attribute msg
medium =
    set (\attributes -> { attributes | size = Medium })


{-| -}
large : Attribute msg
large =
    set (\attributes -> { attributes | size = Large })


{-| For use in Modal footers (adds `large` and `Css.marginTop (Css.px 15)`)
-}
modal : Attribute msg
modal =
    set
        (\attributes ->
            { attributes
                | size = Large
                , customStyles = List.append attributes.customStyles [ Css.marginTop (Css.px 15) ]
            }
        )


type Size
    = Inherited
    | Small
    | Medium
    | Large


type Kind
    = Button
    | Link


type IconPosition
    = Left
    | Right


{-| -}
icon : Svg -> Attribute msg
icon icon_ =
    set (\attributes -> { attributes | icon = Just icon_ })


{-| -}
rightIcon : Svg -> Attribute msg
rightIcon icon_ =
    set (\attributes -> { attributes | rightIcon = Just icon_ })


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying Button styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute msg) -> Attribute msg
custom attributes =
    set
        (\config ->
            { config
                | customAttributes = List.append config.customAttributes attributes
            }
        )


{-| -}
nriDescription : String -> Attribute msg
nriDescription description =
    custom [ ExtraAttributes.nriDescription description ]


{-| -}
testId : String -> Attribute msg
testId id_ =
    custom [ ExtraAttributes.testId id_ ]


{-| -}
id : String -> Attribute msg
id id_ =
    custom [ Attributes.id id_ ]


{-| Hide the left-side icon for the mobile breakpoint.
-}
hideIconForMobile : Attribute msg
hideIconForMobile =
    hideIconFor MediaQuery.mobile


{-| Hide the left-side icon for an arbitrary media query.
-}
hideIconFor : MediaQuery -> Attribute msg
hideIconFor mediaQuery =
    set
        (\config ->
            { config
                | iconStyles =
                    List.append config.iconStyles
                        [ Css.Media.withMedia [ mediaQuery ]
                            [ Css.display Css.none
                            ]
                        ]
            }
        )


{-| -}
hideTextForMobile : Attribute msg
hideTextForMobile =
    hideTextFor MediaQuery.mobile


{-| -}
hideTextFor : MediaQuery -> Attribute msg
hideTextFor mediaQuery =
    css
        [ Css.Media.withMedia [ mediaQuery ]
            [ Css.borderStyle Css.none |> Css.important
            , Css.Global.descendants
                [ ExtraAttributes.nriDescriptionSelector (dataDescriptor "label")
                    [ invisibleStyle
                    ]
                ]
            ]
        ]


{-| -}
css : List Style -> Attribute msg
css styles =
    set
        (\config ->
            { config
                | customStyles = List.append config.customStyles styles
            }
        )


{-| Equivalent to:

    ClickableText.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.notMobile ] styles ]

-}
notMobileCss : List Style -> Attribute msg
notMobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.notMobile ] styles ]


{-| Equivalent to:

    ClickableText.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.mobile ] styles ]

-}
mobileCss : List Style -> Attribute msg
mobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.mobile ] styles ]


{-| Equivalent to:

    ClickableText.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.quizEngineMobile ] styles ]

-}
quizEngineMobileCss : List Style -> Attribute msg
quizEngineMobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.quizEngineMobile ] styles ]



-- LINKING, CLICKING, and TRACKING BEHAVIOR


{-| -}
onClick : msg -> Attribute msg
onClick msg =
    set (ClickableAttributes.onClick msg)


{-| By default, buttons have type "button". Use this attribute to change the button type to "submit".

Note: this attribute is not supported by links.

-}
submit : Attribute msg
submit =
    set ClickableAttributes.submit


{-| Use this attribute when interacting with the button will launch a modal.
-}
opensModal : Attribute msg
opensModal =
    set ClickableAttributes.opensModal


{-| -}
href : String -> Attribute msg
href url =
    set (ClickableAttributes.href url)


{-| Use this link for routing within a single page app.

This will make a normal <a> tag, but change the Events.onClick behavior to avoid reloading the page.

See <https://github.com/elm-lang/html/issues/110> for details on this implementation.

-}
linkSpa : String -> Attribute msg
linkSpa url =
    set (ClickableAttributes.linkSpa url)


{-| -}
linkWithMethod : { method : String, url : String } -> Attribute msg
linkWithMethod config =
    set (ClickableAttributes.linkWithMethod config)


{-| -}
linkWithTracking : { track : msg, url : String } -> Attribute msg
linkWithTracking config =
    set (ClickableAttributes.linkWithTracking config)


{-| -}
linkExternal : String -> Attribute msg
linkExternal url =
    set (ClickableAttributes.linkExternal url)


{-| -}
linkExternalWithTracking : { track : msg, url : String } -> Attribute msg
linkExternalWithTracking config =
    set (ClickableAttributes.linkExternalWithTracking config)


{-| Shows inactive styling.

If a button, this attribute will disable it as you'd expect.

If a link, this attribute will follow the pattern laid out in [Scott O'Hara's disabled links](https://www.scottohara.me/blog/2021/05/28/disabled-links.html) article,
and essentially make the anchor a disabled placeholder.

_Caveat!_

The Component Catalog example will NOT work correctly because of <https://github.com/elm/browser/issues/34>, which describes a problem where "a tags without href generate a navigation event".

In most cases, if you're not using Browser.application, disabled links should work just fine.

-}
disabled : Bool -> Attribute msg
disabled value =
    set (\attributes -> { attributes | disabled = value })


{-| Specifies whether it should have inline appearance.
-}
appearsInline : Attribute msg
appearsInline =
    set
        (\config ->
            { config
                | customStyles =
                    List.append config.customStyles
                        [ Css.borderBottom3 (Css.px 1) Css.solid Colors.azure
                        , Css.Global.withAttribute "aria-disabled=true" [ Css.borderBottom3 (Css.px 1) Css.solid Colors.gray45 ]
                        , Css.disabled [ Css.borderBottom3 (Css.px 1) Css.solid Colors.gray45 ]
                        , Css.display Css.inline
                        , Css.fontFamily Css.inherit
                        , Css.fontWeight Css.inherit
                        , Css.fontSize Css.inherit
                        , Css.Global.descendants
                            [ Css.Global.selector "svg"
                                [ Css.top (Css.em 0.08)
                                ]
                            ]
                        ]
                , size = Inherited
            }
        )


{-| Specifies custom styles for the rightIcon
-}
rightIconCss : List Css.Style -> Attribute msg
rightIconCss styles =
    set
        (\config ->
            { config
                | rightIconStyles =
                    List.append config.rightIconStyles styles
            }
        )


{-| Creates a `<button>` element
-}
button :
    String
    -> List (Attribute msg)
    -> Html msg
button label_ attributes =
    let
        config =
            (label label_ :: attributes)
                |> List.foldl (\(Attribute attribute) b -> attribute b) defaults
    in
    Nri.Ui.styled Html.button
        (dataDescriptor "button")
        (clickableTextSharedStyles config.disabled ++ clickableTextButtonStyles ++ config.customStyles)
        (ClickableAttributes.toButtonAttributes config.clickableAttributes
            { disabled = config.disabled }
            ++ config.customAttributes
        )
        [ viewContent config Button ]


{-| Creates a `<a>` element
-}
link :
    String
    -> List (Attribute msg)
    -> Html msg
link label_ attributes =
    let
        config =
            (label label_ :: attributes)
                |> List.foldl (\(Attribute attribute) l -> attribute l) defaults

        ( name, clickableAttributes ) =
            ClickableAttributes.toLinkAttributes
                { routeToString = identity
                , isDisabled = config.disabled
                }
                config.clickableAttributes
    in
    Nri.Ui.styled Html.a
        (dataDescriptor name)
        (clickableTextSharedStyles config.disabled ++ clickableTextLinkStyles ++ config.customStyles)
        (clickableAttributes ++ config.customAttributes)
        [ viewContent config Link ]


viewContent :
    { a
        | label : String
        , size : Size
        , icon : Maybe Svg
        , rightIcon : Maybe Svg
        , iconStyles : List Style
        , rightIconStyles : List Style
    }
    -> Kind
    -> Html msg
viewContent config kind =
    let
        fontSize =
            case config.size of
                Inherited ->
                    Css.fontSize Css.inherit

                Small ->
                    Css.fontSize (Css.px 13)

                Medium ->
                    Css.fontSize (Css.px 15)

                Large ->
                    Css.fontSize (Css.px 18)

        iconMarginRight =
            case config.size of
                Inherited ->
                    Css.marginRight (Css.em 0.2)

                Small ->
                    Css.marginRight (Css.px 3)

                Medium ->
                    Css.marginRight (Css.px 3)

                Large ->
                    Css.marginRight (Css.px 4)

        iconMarginLeft =
            case config.size of
                Inherited ->
                    Css.marginLeft (Css.em 0.2)

                Small ->
                    Css.marginLeft (Css.px 3)

                Medium ->
                    Css.marginLeft (Css.px 3)

                Large ->
                    Css.marginLeft (Css.px 4)

        viewIcon position icon_ =
            icon_
                |> Svg.withCss
                    ([ Css.width (Css.em 1)
                     , Css.height (Css.em 1)
                     , Css.position Css.relative
                     , Css.top (Css.em 0.15)
                     ]
                        ++ (case position of
                                Left ->
                                    (case kind of
                                        Button ->
                                            [ -- Position absolute makes the parent ignore the icon's height, preventing misalignment with other inline text
                                              Css.position Css.absolute
                                            , Css.left Css.zero
                                            ]

                                        Link ->
                                            [ iconMarginRight ]
                                    )
                                        ++ config.iconStyles

                                Right ->
                                    (case kind of
                                        Button ->
                                            [ Css.position Css.absolute
                                            , Css.right Css.zero
                                            ]

                                        Link ->
                                            [ iconMarginLeft ]
                                    )
                                        ++ config.rightIconStyles
                           )
                    )
                |> Svg.toHtml

        iconAndTextContainer =
            span
                [ Attributes.css
                    (fontSize
                        :: (case kind of
                                Button ->
                                    [ Css.display Css.inlineFlex
                                    , Css.alignItems Css.center
                                    , Css.position Css.relative
                                    ]

                                Link ->
                                    []
                           )
                    )
                ]
                >> List.singleton
    in
    span [ Attributes.css [ fontSize ] ]
        (case ( config.icon, config.rightIcon ) of
            ( Just leftIcon, Just rightIcon_ ) ->
                iconAndTextContainer
                    [ viewIcon Left leftIcon
                    , span
                        [ ExtraAttributes.nriDescription (dataDescriptor "label")
                        , Attributes.css
                            (case kind of
                                Button ->
                                    [ Css.paddingLeft (Css.em 1.3)
                                    , Css.paddingRight (Css.em 1.3)
                                    ]

                                Link ->
                                    []
                            )
                        ]
                        [ text config.label ]
                    , viewIcon Right rightIcon_
                    ]

            ( Just leftIcon, Nothing ) ->
                iconAndTextContainer
                    [ viewIcon Left leftIcon
                    , span
                        [ ExtraAttributes.nriDescription (dataDescriptor "label")
                        , Attributes.css
                            (case kind of
                                Button ->
                                    [ Css.paddingLeft (Css.em 1.3)
                                    ]

                                Link ->
                                    []
                            )
                        ]
                        [ text config.label ]
                    ]

            ( Nothing, Just rightIcon_ ) ->
                iconAndTextContainer
                    [ span
                        [ ExtraAttributes.nriDescription (dataDescriptor "label")
                        , Attributes.css
                            (case kind of
                                Button ->
                                    [ Css.paddingRight (Css.em 1.3)
                                    ]

                                Link ->
                                    []
                            )
                        ]
                        [ text config.label ]
                    , viewIcon Right rightIcon_
                    ]

            ( Nothing, Nothing ) ->
                [ text config.label ]
        )


clickableTextSharedStyles : Bool -> List Css.Style
clickableTextSharedStyles isDisabled =
    let
        baseStyles =
            [ Fonts.baseFont
            , Css.fontWeight (Css.int 600)
            ]
    in
    if isDisabled then
        Css.cursor Css.notAllowed
            :: Css.color Colors.gray45
            :: baseStyles

    else
        Css.cursor Css.pointer
            :: Css.color Colors.azure
            :: Css.hover [ Css.color Colors.azureDark ]
            :: baseStyles


clickableTextLinkStyles : List Css.Style
clickableTextLinkStyles =
    [ Css.textDecoration Css.none
    , Css.display Css.inlineBlock
    ]


clickableTextButtonStyles : List Css.Style
clickableTextButtonStyles =
    [ Css.margin Css.zero
    , Css.padding Css.zero
    , Css.borderStyle Css.none
    , Css.backgroundColor Css.transparent
    , Css.textAlign Css.left
    ]


dataDescriptor : String -> String
dataDescriptor descriptor =
    "clickable-text-" ++ descriptor



--  Internals


type alias ClickableTextAttributes msg =
    { clickableAttributes : ClickableAttributes String msg
    , label : String
    , size : Size
    , icon : Maybe Svg
    , iconStyles : List Style
    , rightIcon : Maybe Svg
    , rightIconStyles : List Style
    , customAttributes : List (Html.Attribute msg)
    , customStyles : List Style
    , disabled : Bool
    }


defaults : ClickableTextAttributes msg
defaults =
    { clickableAttributes = ClickableAttributes.init
    , size = Medium
    , label = ""
    , icon = Nothing
    , iconStyles = []
    , rightIcon = Nothing
    , rightIconStyles = []
    , customAttributes = [ Attributes.class FocusRing.customClass ]
    , customStyles = [ Css.pseudoClass "focus-visible" (Css.borderRadius (Css.px 4) :: FocusRing.tightStyles) ]
    , disabled = False
    }


{-| -}
type Attribute msg
    = Attribute (ClickableTextAttributes msg -> ClickableTextAttributes msg)


set :
    (ClickableTextAttributes msg -> ClickableTextAttributes msg)
    -> Attribute msg
set with =
    Attribute (\config -> with config)
