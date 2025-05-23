module Nri.Ui.ClickableSvg.V2 exposing
    ( button, link
    , Attribute
    , onClick, submit, opensModal
    , href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking
    , exactSize, exactWidth, exactHeight
    , disabled
    , rightIcon
    , iconForMobile, iconForQuizEngineMobile, iconForNarrowMobile
    , withBorder
    , primary, secondary, tertiary, quaternary, danger, dangerSecondary
    , custom, nriDescription, testId, id
    , css, notMobileCss, mobileCss, quizEngineMobileCss
    , small, medium, large
    )

{-|


# Patch changes:

    - adds `nriDescription`, `testId`, and `id` helpers
    - adds `iconForMobile`, `iconForQuizEngineMobile`, `iconForNarrowMobile`
    - adds `submit` and `opensModal`
    - replaces the `disabled` attribute with `aria-disabled="true"`
    - removes click handler from disabled buttons
    - prevents default behavior for disabled submit buttons by setting `type="button"`


# Create a button or link

@docs button, link
@docs Attribute


## Behavior

@docs onClick, submit, opensModal
@docs href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking


## Sizing

@docs exactSize, exactWidth, exactHeight


## State

@docs disabled


## Icons

@docs rightIcon
@docs iconForMobile, iconForQuizEngineMobile, iconForNarrowMobile


## Customization

@docs withBorder
@docs primary, secondary, tertiary, quaternary, danger, dangerSecondary

@docs custom, nriDescription, testId, id


### CSS

@docs css, notMobileCss, mobileCss, quizEngineMobileCss


### DEPRECATED

In practice, we don't use these sizes. Remove them!

@docs small, medium, large

-}

import Accessibility.Styled.Aria as Aria
import ClickableAttributes exposing (ClickableAttributes)
import Css exposing (Color, Style)
import Css.Media
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1 as Svg exposing (Svg)


{-| -}
type Attribute msg
    = Attribute (ButtonOrLink msg -> ButtonOrLink msg)


{-| -}
button : String -> Svg -> List (Attribute msg) -> Html msg
button name icon attributes =
    attributes
        |> List.foldl (\(Attribute attribute) b -> attribute b) (build name icon)
        |> renderButton


{-| -}
link : String -> Svg -> List (Attribute msg) -> Html msg
link name icon attributes =
    attributes
        |> List.foldl (\(Attribute attribute) b -> attribute b) (build name icon)
        |> renderLink



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
    set (ClickableAttributes.linkExternalInternal url)


{-| -}
linkExternalWithTracking : { track : msg, url : String } -> Attribute msg
linkExternalWithTracking config =
    set (ClickableAttributes.linkExternalWithTrackingInternal config)



-- SIZING


type Size
    = Small
    | Medium
    | Large


{-| This is the default. This attribute will be removed in the next version of ClickableSvg!
-}
small : Attribute msg
small =
    set (\attributes -> { attributes | size = Small })


{-| This attribute will be removed in the next version of ClickableSvg!
-}
medium : Attribute msg
medium =
    set (\attributes -> { attributes | size = Medium })


{-| This attribute will be removed in the next version of ClickableSvg!
-}
large : Attribute msg
large =
    set (\attributes -> { attributes | size = Large })


{-| Set the size in `px` for the element's width and height.

Equivalent to:

    [ exactWidth inPx
    , exactHeight inPx
    ]

-}
exactSize : Int -> Attribute msg
exactSize inPx =
    set
        (\attributes ->
            { attributes
                | width = Just (toFloat inPx)
                , height = Just (toFloat inPx)
            }
        )


{-| Define a size in `px` for the element's total width.
-}
exactWidth : Int -> Attribute msg
exactWidth inPx =
    set (\attributes -> { attributes | width = Just (toFloat inPx) })


{-| Define a size in `px` for the element's total height.
-}
exactHeight : Int -> Attribute msg
exactHeight inPx =
    set (\attributes -> { attributes | height = Just (toFloat inPx) })



-- STATE


{-| -}
disabled : Bool -> Attribute msg
disabled disabled_ =
    set (\attributes -> { attributes | disabled = disabled_ })



-- CUSTOMIZATION


{-| Display a border around the icon.
-}
withBorder : Attribute msg
withBorder =
    set (\config -> { config | hasBorder = True })


type Theme
    = Primary
    | Secondary
    | Tertiary
    | Quaternary
    | Danger
    | DangerSecondary


type alias AppliedTheme =
    { main_ : Color
    , mainHovered : Color
    , background : Color
    , backgroundHovered : Color
    , includeBorder : Bool
    , borderColor : Color
    , borderBottom : Color
    , borderHover : Color
    }


disabledTheme : AppliedTheme
disabledTheme =
    { main_ = Colors.gray75
    , mainHovered = Colors.gray75
    , background = Colors.white
    , backgroundHovered = Colors.white
    , includeBorder = True
    , borderColor = Colors.gray75
    , borderBottom = Colors.gray75
    , borderHover = Colors.gray75
    }


applyTheme : Theme -> AppliedTheme
applyTheme theme =
    case theme of
        Primary ->
            { main_ = Colors.white
            , mainHovered = Colors.white
            , background = Colors.azure
            , backgroundHovered = Colors.azureDark
            , includeBorder = False
            , borderColor = Colors.white
            , borderBottom = Colors.azureDark
            , borderHover = Colors.azureDark
            }

        Secondary ->
            { main_ = Colors.azure
            , mainHovered = Colors.azureDark
            , background = Colors.white
            , backgroundHovered = Colors.glacier
            , includeBorder = True
            , borderColor = Colors.azure
            , borderBottom = Colors.azure
            , borderHover = Colors.azure
            }

        Tertiary ->
            { main_ = Colors.navy
            , mainHovered = Colors.navy
            , background = Colors.white
            , backgroundHovered = Colors.frost
            , includeBorder = True
            , borderColor = Colors.gray75
            , borderBottom = Colors.gray75
            , borderHover = Colors.gray75
            }

        Quaternary ->
            { main_ = Colors.gray45
            , mainHovered = Colors.azure
            , background = Colors.gray96
            , backgroundHovered = Colors.glacier
            , includeBorder = True
            , borderColor = Colors.gray92
            , borderBottom = Colors.gray92
            , borderHover = Colors.azure
            }

        Danger ->
            { main_ = Colors.white
            , mainHovered = Colors.white
            , background = Colors.red
            , backgroundHovered = Colors.redDark
            , includeBorder = False
            , borderColor = Colors.white
            , borderBottom = Colors.redDark
            , borderHover = Colors.redDark
            }

        DangerSecondary ->
            { main_ = Colors.red
            , mainHovered = Colors.redDark
            , background = Colors.white
            , backgroundHovered = Colors.redLight
            , includeBorder = True
            , borderColor = Colors.red
            , borderBottom = Colors.red
            , borderHover = Colors.red
            }


{-| white/transparent icon on an azure background.
-}
primary : Attribute msg
primary =
    set (\attributes -> { attributes | theme = Primary })


{-| This is the default: a blue icon on a transparent background, or a blue icon
on a white/glacier icon with a blue border.
-}
secondary : Attribute msg
secondary =
    set (\attributes -> { attributes | theme = Secondary })


{-| Used to de-emphasize elements when not hovered.
-}
tertiary : Attribute msg
tertiary =
    set (\attributes -> { attributes | theme = Tertiary })


{-| Used to de-emphasize elements when not hovered.
-}
quaternary : Attribute msg
quaternary =
    set (\attributes -> { attributes | theme = Quaternary })


{-| White/transparent icon on a red background.
-}
danger : Attribute msg
danger =
    set (\attributes -> { attributes | theme = Danger })


{-| Red icon on a white/transparent background.
-}
dangerSecondary : Attribute msg
dangerSecondary =
    set (\attributes -> { attributes | theme = DangerSecondary })


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

    ClickableSvg.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.notMobile ] styles ]

-}
notMobileCss : List Style -> Attribute msg
notMobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.notMobile ] styles ]


{-| Equivalent to:

    ClickableSvg.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.mobile ] styles ]

-}
mobileCss : List Style -> Attribute msg
mobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.mobile ] styles ]


{-| Equivalent to:

    ClickableSvg.css
        [ Css.Media.withMedia [ Nri.Ui.MediaQuery.V1.quizEngineMobile ] styles ]

-}
quizEngineMobileCss : List Style -> Attribute msg
quizEngineMobileCss styles =
    css [ Css.Media.withMedia [ MediaQuery.quizEngineMobile ] styles ]


{-| -}
rightIcon : Svg -> Attribute msg
rightIcon icon =
    set (\config -> { config | rightIcon = Just icon })


{-| -}
iconForMobile : Svg -> Attribute msg
iconForMobile icon =
    set (\config -> { config | iconForMobile = Just icon })


{-| -}
iconForQuizEngineMobile : Svg -> Attribute msg
iconForQuizEngineMobile icon =
    set (\config -> { config | iconForQuizEngineMobile = Just icon })


{-| -}
iconForNarrowMobile : Svg -> Attribute msg
iconForNarrowMobile icon =
    set (\config -> { config | iconForNarrowMobile = Just icon })



-- INTERNALS


set :
    (ButtonOrLinkAttributes msg -> ButtonOrLinkAttributes msg)
    -> Attribute msg
set with =
    Attribute (\(ButtonOrLink config) -> ButtonOrLink (with config))


build : String -> Svg -> ButtonOrLink msg
build label icon =
    ButtonOrLink
        { clickableAttributes = ClickableAttributes.init
        , label = label
        , icon = icon
        , iconForMobile = Nothing
        , iconForQuizEngineMobile = Nothing
        , iconForNarrowMobile = Nothing
        , rightIcon = Nothing
        , disabled = False
        , size = Small
        , width = Nothing
        , height = Nothing
        , customAttributes = []
        , customStyles = []
        , hasBorder = False
        , theme = Secondary
        }


type ButtonOrLink msg
    = ButtonOrLink (ButtonOrLinkAttributes msg)


type alias ButtonOrLinkAttributes msg =
    { clickableAttributes : ClickableAttributes String msg
    , label : String
    , icon : Svg
    , iconForMobile : Maybe Svg
    , iconForQuizEngineMobile : Maybe Svg
    , iconForNarrowMobile : Maybe Svg
    , rightIcon : Maybe Svg
    , disabled : Bool
    , size : Size
    , width : Maybe Float
    , height : Maybe Float
    , customAttributes : List (Html.Attribute msg)
    , customStyles : List Style
    , hasBorder : Bool
    , theme : Theme
    }


renderButton : ButtonOrLink msg -> Html msg
renderButton ((ButtonOrLink config) as button_) =
    let
        theme =
            if config.disabled then
                disabledTheme

            else
                applyTheme config.theme
    in
    Html.button
        ([ Attributes.class "Nri-Ui-Clickable-Svg-V1__button"
         , Attributes.class FocusRing.customClass
         , Attributes.css (buttonOrLinkStyles config theme ++ config.customStyles)
         , Aria.label config.label
         ]
            ++ ClickableAttributes.toButtonAttributes config.clickableAttributes
                { disabled = config.disabled }
            ++ config.customAttributes
        )
        (renderIcons config theme.includeBorder)


renderLink : ButtonOrLink msg -> Html msg
renderLink ((ButtonOrLink config) as link_) =
    let
        ( linkFunctionName, extraAttrs ) =
            ClickableAttributes.toLinkAttributes
                { routeToString = identity
                , isDisabled = config.disabled
                }
                config.clickableAttributes

        theme =
            if config.disabled then
                disabledTheme

            else
                applyTheme config.theme
    in
    Html.a
        ([ Attributes.class ("Nri-Ui-Clickable-Svg-" ++ linkFunctionName)
         , Attributes.class FocusRing.customClass
         , Attributes.css (buttonOrLinkStyles config theme ++ config.customStyles)
         , Aria.label config.label
         ]
            ++ extraAttrs
            ++ config.customAttributes
        )
        (renderIcons config theme.includeBorder)


renderIcons : ButtonOrLinkAttributes msg -> Bool -> List (Html msg)
renderIcons config includeBorder =
    let
        size =
            getSize config.size

        bordersAndPadding =
            getBorder config.size config.width config.height includeBorder

        iconWidth =
            if config.hasBorder then
                Maybe.withDefault size config.width
                    - bordersAndPadding.leftPadding
                    - bordersAndPadding.rightPadding
                    - bordersAndPadding.leftBorder
                    - bordersAndPadding.rightBorder

            else
                Maybe.withDefault size config.width

        iconHeight =
            if config.hasBorder then
                Maybe.withDefault size config.height
                    - bordersAndPadding.topPadding
                    - bordersAndPadding.bottomPadding
                    - bordersAndPadding.topBorder
                    - bordersAndPadding.bottomBorder

            else
                Maybe.withDefault size config.height

        iconStyles =
            case config.rightIcon of
                Just _ ->
                    [ Css.width (Css.px (iconWidth * 3 / 5 - 1))
                    , Css.height (Css.px (iconWidth * 3 / 5 - 1))
                    , Css.marginRight (Css.px 1)
                    ]

                Nothing ->
                    [ Css.displayFlex
                    , Css.maxWidth (Css.px iconWidth)
                    , Css.maxHeight (Css.px iconHeight)
                    , Css.height (Css.pct 100)
                    , Css.margin Css.auto
                    ]

        renderUnless breakpoints =
            Svg.withCss
                [ Css.batch iconStyles
                , Css.Media.withMedia breakpoints
                    [ Css.display Css.none
                    ]
                ]
                >> Svg.toHtml
                >> Just

        renderRightIcon =
            Svg.withCss
                [ Css.width (Css.px (iconWidth * 2 / 5 - 4))
                , Css.height (Css.px (iconWidth * 2 / 5 - 4))
                , Css.marginLeft (Css.px 4)
                ]
                >> Svg.toHtml
    in
    case ( config.iconForNarrowMobile, config.iconForQuizEngineMobile, config.iconForMobile ) of
        ( Just iconForNarrowMobile_, Just iconForQuizEngineMobile_, Nothing ) ->
            [ renderUnless [ MediaQuery.quizEngineMobile ] config.icon
            , renderUnless [ MediaQuery.narrowMobile, MediaQuery.notQuizEngineMobile ]
                iconForQuizEngineMobile_
            , renderUnless [ MediaQuery.notNarrowMobile ] iconForNarrowMobile_
            , Maybe.map renderRightIcon config.rightIcon
            ]
                |> List.filterMap identity

        ( Just iconForNarrowMobile_, Just iconForQuizEngineMobile_, Just iconForMobile_ ) ->
            [ renderUnless [ MediaQuery.mobile ] config.icon
            , renderUnless [ MediaQuery.quizEngineMobile, MediaQuery.notMobile ]
                iconForMobile_
            , renderUnless [ MediaQuery.narrowMobile, MediaQuery.notQuizEngineMobile ]
                iconForQuizEngineMobile_
            , renderUnless [ MediaQuery.notNarrowMobile ] iconForNarrowMobile_
            , Maybe.map renderRightIcon config.rightIcon
            ]
                |> List.filterMap identity

        ( Just iconForNarrowMobile_, Nothing, Just iconForMobile_ ) ->
            [ renderUnless [ MediaQuery.mobile ] config.icon
            , renderUnless [ MediaQuery.narrowMobile, MediaQuery.notMobile ] iconForMobile_
            , renderUnless [ MediaQuery.notNarrowMobile ] iconForNarrowMobile_
            , Maybe.map renderRightIcon config.rightIcon
            ]
                |> List.filterMap identity

        ( Just iconForNarrowMobile_, Nothing, Nothing ) ->
            [ renderUnless [ MediaQuery.narrowMobile ] config.icon
            , renderUnless [ MediaQuery.notNarrowMobile ] iconForNarrowMobile_
            , Maybe.map renderRightIcon config.rightIcon
            ]
                |> List.filterMap identity

        ( Nothing, Just iconForQuizEngineMobile_, Nothing ) ->
            [ renderUnless [ MediaQuery.quizEngineMobile ] config.icon
            , renderUnless [ MediaQuery.notQuizEngineMobile ]
                iconForQuizEngineMobile_
            , Maybe.map renderRightIcon config.rightIcon
            ]
                |> List.filterMap identity

        ( Nothing, Just iconForQuizEngineMobile_, Just iconForMobile_ ) ->
            [ renderUnless [ MediaQuery.mobile ] config.icon
            , renderUnless [ MediaQuery.quizEngineMobile, MediaQuery.notMobile ]
                iconForMobile_
            , renderUnless [ MediaQuery.notQuizEngineMobile ]
                iconForQuizEngineMobile_
            , Maybe.map renderRightIcon config.rightIcon
            ]
                |> List.filterMap identity

        ( Nothing, Nothing, Just iconForMobile_ ) ->
            [ renderUnless [ MediaQuery.mobile ] config.icon
            , renderUnless [ MediaQuery.notMobile ] iconForMobile_
            , Maybe.map renderRightIcon config.rightIcon
            ]
                |> List.filterMap identity

        ( Nothing, Nothing, Nothing ) ->
            [ config.icon
                |> Svg.withCss iconStyles
                |> Svg.toHtml
                |> Just
            , Maybe.map renderRightIcon config.rightIcon
            ]
                |> List.filterMap identity


buttonOrLinkStyles : ButtonOrLinkAttributes msg -> AppliedTheme -> List Style
buttonOrLinkStyles config { main_, mainHovered, background, backgroundHovered, borderColor, borderBottom, borderHover, includeBorder } =
    let
        cursor =
            if config.disabled then
                Css.notAllowed

            else
                Css.pointer

        bordersAndPadding =
            getBorder config.size config.width config.height includeBorder
    in
    [ Css.property "transition"
        "background-color 0.2s, color 0.2s, border-width 0s, border-color 0.2s"

    -- Colors, text decoration, cursor
    , Css.textDecoration Css.none
    , Css.color main_
    , Css.visited [ Css.color main_ ]
    , Css.hover
        [ Css.textDecoration Css.none
        , Css.color mainHovered
        , Css.cursor cursor
        ]

    -- Margins, borders, padding
    , Css.margin Css.zero
    , Css.textAlign Css.center
    , Css.batch <|
        if config.hasBorder then
            [ Css.borderRadius (Css.px 8)
            , Css.borderColor borderColor
            , Css.borderBottomColor borderBottom
            , Css.borderStyle Css.solid
            , if includeBorder then
                Css.batch
                    [ Css.borderTopWidth (Css.px bordersAndPadding.topBorder)
                    , Css.borderRightWidth (Css.px bordersAndPadding.rightBorder)
                    , Css.borderLeftWidth (Css.px bordersAndPadding.leftBorder)
                    ]

              else
                Css.borderWidth Css.zero
            , Css.borderBottomWidth (Css.px bordersAndPadding.bottomBorder)
            , Css.backgroundColor background
            , Css.hover
                [ Css.borderColor borderHover
                , Css.backgroundColor backgroundHovered
                ]
            , Css.padding4
                (Css.px bordersAndPadding.topPadding)
                (Css.px bordersAndPadding.rightPadding)
                (Css.px bordersAndPadding.bottomPadding)
                (Css.px bordersAndPadding.leftPadding)
            , Css.height (Css.px (getSize config.size))
            ]

        else
            [ Css.borderWidth Css.zero
            , Css.padding Css.zero
            , Css.backgroundColor Css.transparent
            ]

    -- Sizing
    , Css.boxSizing Css.borderBox
    , Css.batch <|
        case config.rightIcon of
            Just _ ->
                [ Css.display Css.inlineFlex
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                , Css.minWidth (Css.px (Maybe.withDefault (getSize config.size) config.width))
                , Css.minHeight (Css.px (Maybe.withDefault (getSize config.size) config.height))
                ]

            Nothing ->
                [ Css.display Css.inlineBlock
                , Css.width (Css.px (Maybe.withDefault (getSize config.size) config.width))
                , Css.height (Css.px (Maybe.withDefault (getSize config.size) config.height))
                ]

    -- Focus
    , Css.pseudoClass "focus-visible"
        (if config.hasBorder then
            [ Css.outline3 (Css.px 2) Css.solid Css.transparent, FocusRing.boxShadows [] ]

         else
            FocusRing.styles
        )
    ]


getSize : Size -> Float
getSize size =
    case size of
        Small ->
            smallSize

        Medium ->
            mediumSize

        Large ->
            largeSize


smallSize : Float
smallSize =
    35


mediumSize : Float
mediumSize =
    45


largeSize : Float
largeSize =
    55


getBorder :
    Size
    -> Maybe Float
    -> Maybe Float
    -> Bool
    ->
        { topBorder : Float
        , topPadding : Float
        , rightBorder : Float
        , rightPadding : Float
        , bottomBorder : Float
        , bottomPadding : Float
        , leftBorder : Float
        , leftPadding : Float
        }
getBorder size width height includeBorder =
    let
        w =
            Maybe.withDefault (getSize size) width

        h =
            Maybe.withDefault (getSize size) height

        verticalSettings =
            if h < smallSize then
                -- Teeny size vertical settings
                { topBorder = 1
                , topPadding = 1
                , bottomBorder = 2
                , bottomPadding = 1
                }

            else if h < mediumSize then
                -- Small size vertical settings
                { topBorder = 1
                , topPadding = 7
                , bottomBorder = 2
                , bottomPadding = 7
                }

            else if h < largeSize then
                -- Medium size vertical settings
                { topBorder = 1
                , topPadding = 10
                , bottomBorder = 3
                , bottomPadding = 10
                }

            else
                -- Large size vertical settings
                { topBorder = 1
                , topPadding = 13
                , bottomBorder = 4
                , bottomPadding = 13
                }

        horizontalSettings =
            if w < smallSize then
                -- Teeny size horizontal settings
                { rightBorder = 1
                , rightPadding = 2
                , leftBorder = 1
                , leftPadding = 2
                }

            else if w < mediumSize then
                -- Small size horizontal settings
                { rightBorder = 1
                , rightPadding = 7
                , leftBorder = 1
                , leftPadding = 7
                }

            else if w < largeSize then
                -- Medium size horizontal settings
                { rightBorder = 1
                , rightPadding = 9
                , leftBorder = 1
                , leftPadding = 9
                }

            else
                -- Large size horizontal settings
                { rightBorder = 1
                , rightPadding = 12
                , leftBorder = 1
                , leftPadding = 12
                }

        orZero value =
            if includeBorder then
                value

            else
                0
    in
    { topBorder = orZero verticalSettings.topBorder
    , bottomBorder = verticalSettings.bottomBorder
    , rightBorder = orZero horizontalSettings.rightBorder
    , leftBorder = orZero horizontalSettings.leftBorder
    , topPadding = verticalSettings.topPadding
    , bottomPadding = verticalSettings.bottomPadding
    , rightPadding = horizontalSettings.rightPadding
    , leftPadding = horizontalSettings.leftPadding
    }
