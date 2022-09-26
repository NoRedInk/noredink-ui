module Nri.Ui.ClickableSvg.V2 exposing
    ( button, link
    , Attribute
    , onClick
    , href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking
    , exactSize, exactWidth, exactHeight
    , disabled
    , withBorder
    , primary, secondary, tertiary, danger, dangerSecondary
    , custom, nriDescription, testId, id
    , css, notMobileCss, mobileCss, quizEngineMobileCss
    , iconForMobile, iconForQuizEngineMobile, iconForNarrowMobile
    , small, medium, large
    )

{-|


# Patch changes:

    - adds `nriDescription`, `testId`, and `id` helpers
    - adds `iconForMobile`, `iconForQuizEngineMobile`, `iconForNarrowMobile`


# Create a button or link

@docs button, link
@docs Attribute


## Behavior

@docs onClick
@docs href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking


## Sizing

@docs exactSize, exactWidth, exactHeight


## State

@docs disabled


## Customization

@docs withBorder
@docs primary, secondary, tertiary, danger, dangerSecondary

@docs custom, nriDescription, testId, id


### CSS

@docs css, notMobileCss, mobileCss, quizEngineMobileCss
@docs iconForMobile, iconForQuizEngineMobile, iconForNarrowMobile


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


setClickableAttributes :
    (ClickableAttributes String msg -> ClickableAttributes String msg)
    -> Attribute msg
setClickableAttributes apply =
    set
        (\attributes ->
            { attributes | clickableAttributes = apply attributes.clickableAttributes }
        )


{-| -}
onClick : msg -> Attribute msg
onClick msg =
    setClickableAttributes (ClickableAttributes.onClick msg)


{-| -}
href : String -> Attribute msg
href url =
    setClickableAttributes (ClickableAttributes.href url)


{-| Use this link for routing within a single page app.

This will make a normal <a> tag, but change the Events.onClick behavior to avoid reloading the page.

See <https://github.com/elm-lang/html/issues/110> for details on this implementation.

-}
linkSpa : String -> Attribute msg
linkSpa url =
    setClickableAttributes (ClickableAttributes.linkSpa url)


{-| -}
linkWithMethod : { method : String, url : String } -> Attribute msg
linkWithMethod config =
    setClickableAttributes (ClickableAttributes.linkWithMethod config)


{-| -}
linkWithTracking : { track : msg, url : String } -> Attribute msg
linkWithTracking config =
    setClickableAttributes (ClickableAttributes.linkWithTracking config)


{-| -}
linkExternal : String -> Attribute msg
linkExternal url =
    setClickableAttributes (ClickableAttributes.linkExternal url)


{-| -}
linkExternalWithTracking : { track : msg, url : String } -> Attribute msg
linkExternalWithTracking config =
    setClickableAttributes (ClickableAttributes.linkExternalWithTracking config)



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
         , Attributes.type_ "button"
         , Attributes.css (buttonOrLinkStyles config theme ++ config.customStyles)
         , Attributes.disabled config.disabled
         , Aria.label config.label
         ]
            ++ ClickableAttributes.toButtonAttributes config.clickableAttributes
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
         , Aria.disabled config.disabled
         , Aria.label config.label
         ]
            ++ (if not config.disabled then
                    extraAttrs

                else
                    []
               )
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
                size
                    - bordersAndPadding.leftPadding
                    - bordersAndPadding.rightPadding
                    - bordersAndPadding.leftBorder
                    - bordersAndPadding.rightBorder

            else
                Maybe.withDefault size config.width

        iconHeight =
            if config.hasBorder then
                size
                    - bordersAndPadding.topPadding
                    - bordersAndPadding.bottomPadding
                    - bordersAndPadding.topBorder
                    - bordersAndPadding.bottomBorder

            else
                Maybe.withDefault size config.height

        iconStyles =
            [ Css.displayFlex
            , Css.maxWidth (Css.px iconWidth)
            , Css.maxHeight (Css.px iconHeight)
            , Css.height (Css.pct 100)
            , Css.margin Css.auto
            ]

        hideFor breakpoint =
            Svg.withCss
                [ Css.Media.withMedia [ breakpoint ]
                    [ Css.display Css.none
                    ]
                ]
    in
    case config.iconForMobile of
        Just iconForMobile_ ->
            [ config.icon
                |> Svg.withCss iconStyles
                |> hideFor MediaQuery.mobile
                |> Svg.toHtml
            , iconForMobile_
                |> Svg.withCss iconStyles
                |> hideFor MediaQuery.notMobile
                |> Svg.toHtml
            ]

        Nothing ->
            [ config.icon
                |> Svg.withCss iconStyles
                |> Svg.toHtml
            ]


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
    , Css.display Css.inlineBlock
    , Css.boxSizing Css.borderBox
    , Css.width (Css.px (Maybe.withDefault (getSize config.size) config.width))
    , Css.height (Css.px (Maybe.withDefault (getSize config.size) config.height))

    -- Focus
    , Css.pseudoClass "focus-visible"
        (if config.hasBorder then
            [ Css.outline Css.none, FocusRing.boxShadows [] ]

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
    36


mediumSize : Float
mediumSize =
    45


largeSize : Float
largeSize =
    56


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
                , bottomPadding = 8
                }

            else
                -- Large size vertical settings
                { topBorder = 1
                , topPadding = 13
                , bottomBorder = 4
                , bottomPadding = 11
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
