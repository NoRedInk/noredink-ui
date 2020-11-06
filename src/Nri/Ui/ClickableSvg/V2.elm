module Nri.Ui.ClickableSvg.V2 exposing
    ( button, link
    , Attribute
    , onClick
    , href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking
    , small, medium, large
    , exactWidth, exactHeight
    , disabled
    , withBorder
    , primary, secondary, danger, dangerSecondary
    , custom, css, nriDescription, testId, id
    )

{-|

# Patch changes:

    - adds `nriDescription`, `testId`, and `id` helpers


# Create a button or link

@docs button, link
@docs Attribute


## Behavior

@docs onClick
@docs href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking


## Sizing

@docs small, medium, large
@docs exactWidth, exactHeight


## State

@docs disabled


## Customization

@docs withBorder
@docs primary, secondary, danger, dangerSecondary

@docs custom, css, nriDescription, testId, id

-}

import Accessibility.Styled.Widget as Widget
import ClickableAttributes exposing (ClickableAttributes)
import Css exposing (Color, Style)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes
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
    (ClickableAttributes msg -> ClickableAttributes msg)
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


{-| This is the default.
-}
small : Attribute msg
small =
    set (\attributes -> { attributes | size = Small })


{-| -}
medium : Attribute msg
medium =
    set (\attributes -> { attributes | size = Medium })


{-| -}
large : Attribute msg
large =
    set (\attributes -> { attributes | size = Large })


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
    | Danger
    | DangerSecondary


type alias AppliedTheme =
    { main_ : Color
    , mainHovered : Color
    , background : Color
    , backgroundHovered : Color
    , includeBorder : Bool
    , borderBottom : Color
    }


disabledTheme : AppliedTheme
disabledTheme =
    { main_ = Colors.gray75
    , mainHovered = Colors.gray75
    , background = Colors.white
    , backgroundHovered = Colors.white
    , includeBorder = True
    , borderBottom = Colors.gray75
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
            , borderBottom = Colors.azureDark
            }

        Secondary ->
            { main_ = Colors.azure
            , mainHovered = Colors.azureDark
            , background = Colors.white
            , backgroundHovered = Colors.glacier
            , includeBorder = True
            , borderBottom = Colors.azure
            }

        Danger ->
            { main_ = Colors.white
            , mainHovered = Colors.white
            , background = Colors.red
            , backgroundHovered = Colors.redDark
            , includeBorder = False
            , borderBottom = Colors.redDark
            }

        DangerSecondary ->
            { main_ = Colors.red
            , mainHovered = Colors.redDark
            , background = Colors.white
            , backgroundHovered = Colors.redLight
            , includeBorder = True
            , borderBottom = Colors.red
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
    { clickableAttributes : ClickableAttributes msg
    , label : String
    , icon : Svg
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
         , Attributes.type_ "button"
         , Attributes.css (buttonOrLinkStyles config theme ++ config.customStyles)
         , Attributes.disabled config.disabled
         , Widget.label config.label
         ]
            ++ ClickableAttributes.toButtonAttributes config.clickableAttributes
            ++ config.customAttributes
        )
        [ renderIcon config theme.includeBorder
        ]


type Link
    = Default
    | WithTracking
    | SinglePageApp
    | WithMethod String
    | External
    | ExternalWithTracking


renderLink : ButtonOrLink msg -> Html msg
renderLink ((ButtonOrLink config) as link_) =
    let
        ( linkFunctionName, extraAttrs ) =
            ClickableAttributes.toLinkAttributes config.clickableAttributes

        theme =
            if config.disabled then
                disabledTheme

            else
                applyTheme config.theme
    in
    Html.a
        ([ Attributes.class ("Nri-Ui-Clickable-Svg-" ++ linkFunctionName)
         , Attributes.css (buttonOrLinkStyles config theme ++ config.customStyles)
         , Widget.disabled config.disabled
         , Widget.label config.label
         ]
            ++ (if not config.disabled then
                    extraAttrs

                else
                    []
               )
            ++ config.customAttributes
        )
        [ renderIcon config theme.includeBorder
        ]


renderIcon : ButtonOrLinkAttributes msg -> Bool -> Html msg
renderIcon config includeBorder =
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
    in
    config.icon
        |> Svg.withCss
            [ Css.displayFlex
            , Css.maxWidth (Css.px iconWidth)
            , Css.maxHeight (Css.px iconHeight)
            , Css.height (Css.pct 100)
            , Css.margin Css.auto
            ]
        |> Svg.toHtml


buttonOrLinkStyles : ButtonOrLinkAttributes msg -> AppliedTheme -> List Style
buttonOrLinkStyles config { main_, mainHovered, background, backgroundHovered, borderBottom, includeBorder } =
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
            , Css.borderColor main_
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
                [ Css.borderColor borderBottom
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
