module Nri.Ui.Switch.V2 exposing
    ( view, label
    , Attribute
    , containerCss, labelCss, custom, nriDescription, id, testId
    , onSwitch, disabled, enabled
    )

{-|


# Changes from V1:

    - Fixes invalid ARIA use, [conformance requirements](https://www.w3.org/TR/html-aria/#docconformance)
    - labels should only support strings (this is the only way they're actually used in practice)
    - extends API to be more consistent with other form/control components
    - Use Colors values instead of hardcoded hex strings

@docs view, label


### Attributes

@docs Attribute
@docs containerCss, labelCss, custom, nriDescription, id, testId
@docs onSwitch, disabled, enabled

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Widget as Widget
import Css exposing (Color, Style)
import Css.Global as Global
import Css.Media
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui.Colors.Extra exposing (toCssString)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as Extra
import Nri.Ui.Svg.V1 exposing (Svg)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttributes


{-| -}
type Attribute msg
    = Attribute (Config msg -> Config msg)


{-| Specify what happens when the switch is toggled.
-}
onSwitch : (Bool -> msg) -> Attribute msg
onSwitch onSwitch_ =
    Attribute <| \config -> { config | onSwitch = Just onSwitch_ }


{-| Explicitly specify that you want this switch to be disabled. If you don't
specify `onSwitch`, this is the default, but it's provided so you don't have
to resort to `filterMap` or similar to build a clean list of attributes.
-}
disabled : Attribute msg
disabled =
    Attribute <| \config -> { config | isDisabled = True }


{-| -}
enabled : Attribute msg
enabled =
    Attribute <| \config -> { config | isDisabled = False }


{-| Set the HTML ID of the switch toggle. If you have only one on the page,
you don't need to set this, but you should definitely set it if you have
more than one.
-}
id : String -> Attribute msg
id id_ =
    Attribute <| \config -> { config | id = id_ }


{-| Add labeling text to the switch. This text should be descriptive and
able to be displayed inline.
-}
label : String -> Attribute msg
label label_ =
    Attribute <| \config -> { config | label = label_ }


{-| Pass custom attributes through to be attached to the underlying input.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying styles change.
Instead, please use `containerCss` or `labelCss`.

-}
custom : List (Html.Attribute Never) -> Attribute msg
custom custom_ =
    Attribute <| \config -> { config | custom = config.custom ++ custom_ }


{-| -}
nriDescription : String -> Attribute msg
nriDescription description =
    custom [ Extra.nriDescription description ]


{-| -}
testId : String -> Attribute msg
testId id_ =
    custom [ Extra.testId id_ ]


{-| Adds CSS to the Switch container.
-}
containerCss : List Css.Style -> Attribute msg
containerCss styles =
    Attribute <| \config -> { config | containerCss = config.containerCss ++ styles }


{-| Adds CSS to the element containing the label text.

Note that these styles don't apply to the literal HTML label element, since it contains the icon SVG as well.

-}
labelCss : List Css.Style -> Attribute msg
labelCss styles =
    Attribute <| \config -> { config | labelCss = config.labelCss ++ styles }


type alias Config msg =
    { onSwitch : Maybe (Bool -> msg)
    , id : String
    , label : String
    , containerCss : List Style
    , labelCss : List Style
    , isDisabled : Bool
    , custom : List (Html.Attribute Never)
    }


defaultConfig : Config msg
defaultConfig =
    { onSwitch = Nothing
    , id = "nri-ui-switch-with-default-id"
    , label = ""
    , containerCss = []
    , labelCss = []
    , isDisabled = False
    , custom = []
    }


{-| Render a switch. The boolean here indicates whether the switch is on
or not.
-}
view : List (Attribute msg) -> Bool -> Html msg
view attrs isOn =
    let
        config =
            List.foldl (\(Attribute update) -> update) defaultConfig attrs
    in
    Html.label
        [ Attributes.id (config.id ++ "-container")
        , Attributes.css
            [ Css.display Css.inlineFlex
            , Css.alignItems Css.center
            , Css.position Css.relative
            , Css.pseudoClass "focus-within"
                [ Global.descendants
                    [ Global.class "switch-slider"
                        [ stroke Colors.azure
                        , Css.property "stroke-width" "3px"
                        ]
                    ]
                ]
            , Css.cursor
                (if config.onSwitch /= Nothing then
                    Css.pointer

                 else
                    Css.notAllowed
                )
            , Css.batch config.containerCss
            ]
        , Attributes.for config.id
        ]
        [ viewCheckbox
            { id = config.id
            , onCheck = config.onSwitch
            , checked = isOn
            , custom = config.custom
            }
        , Nri.Ui.Svg.V1.toHtml
            (viewSwitch
                { id = config.id
                , isOn = isOn
                , enabled = config.onSwitch /= Nothing
                }
            )
        , Html.span
            [ Attributes.css
                [ Css.fontWeight (Css.int 600)
                , Css.color Colors.navy
                , Css.paddingLeft (Css.px 5)
                , Fonts.baseFont
                , Css.batch config.labelCss
                ]
            ]
            [ Html.text config.label ]
        ]


viewCheckbox :
    { id : String
    , onCheck : Maybe (Bool -> msg)
    , checked : Bool
    , custom : List (Html.Attribute Never)
    }
    -> Html msg
viewCheckbox config =
    Html.checkbox config.id
        (Just config.checked)
        ([ Attributes.id config.id
         , Attributes.css
            [ Css.position Css.absolute
            , Css.top (Css.px 10)
            , Css.left (Css.px 10)
            , Css.zIndex (Css.int 0)
            , Css.opacity (Css.num 0)
            ]
         , case config.onCheck of
            Just onCheck ->
                Events.onCheck onCheck

            Nothing ->
                Widget.disabled True
         ]
            ++ List.map (Attributes.map never) config.custom
        )


viewSwitch :
    { id : String
    , isOn : Bool
    , enabled : Bool
    }
    -> Svg
viewSwitch config =
    let
        shadowFilterId =
            config.id ++ "-shadow-filter"

        shadowBoxId =
            config.id ++ "-shadow-box"
    in
    Svg.svg
        [ SvgAttributes.width "43"
        , SvgAttributes.height "32"
        , SvgAttributes.viewBox "0 0 43 32"
        , SvgAttributes.css
            [ Css.zIndex (Css.int 1)
            , if config.enabled then
                Css.opacity (Css.num 1)

              else
                Css.opacity (Css.num 0.4)
            ]
        ]
        [ Svg.defs []
            [ Svg.filter
                [ SvgAttributes.id shadowFilterId
                , SvgAttributes.width "105%"
                , SvgAttributes.height "106.7%"
                , SvgAttributes.x "-2.5%"
                , SvgAttributes.y "-3.3%"
                , SvgAttributes.filterUnits "objectBoundingBox"
                ]
                [ Svg.feOffset
                    [ SvgAttributes.dy "2"
                    , SvgAttributes.in_ "SourceAlpha"
                    , SvgAttributes.result "shadowOffsetInner1"
                    ]
                    []
                , Svg.feComposite
                    [ SvgAttributes.in_ "shadowOffsetInner1"
                    , SvgAttributes.in2 "SourceAlpha"
                    , SvgAttributes.k2 "-1"
                    , SvgAttributes.k3 "1"
                    , SvgAttributes.operator "arithmetic"
                    , SvgAttributes.result "shadowInnerInner1"
                    ]
                    []
                , Svg.feColorMatrix
                    [ SvgAttributes.in_ "shadowInnerInner1"
                    , SvgAttributes.values "0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0.1 0"
                    ]
                    []
                ]
            , Svg.rect
                [ SvgAttributes.id shadowBoxId
                , SvgAttributes.width "40"
                , SvgAttributes.height "30"
                , SvgAttributes.x "0"
                , SvgAttributes.y "0"
                , SvgAttributes.rx "15"
                ]
                []
            ]
        , Svg.g
            [ SvgAttributes.fill "none"
            , SvgAttributes.fillRule "even-odd"
            , SvgAttributes.transform "translate(1, 1)"
            ]
            [ Svg.g []
                [ Svg.use
                    [ SvgAttributes.xlinkHref ("#" ++ shadowBoxId)
                    , SvgAttributes.css
                        [ if config.isOn then
                            Css.fill Colors.glacier

                          else
                            Css.fill Colors.gray92
                        , transition "fill 0.2s"
                        ]
                    ]
                    []
                , Svg.use
                    [ SvgAttributes.xlinkHref ("#" ++ shadowBoxId)
                    , SvgAttributes.fill "#000"
                    , SvgAttributes.filter ("url(#" ++ shadowFilterId ++ ")")
                    ]
                    []
                ]
            , Svg.g
                [ SvgAttributes.css
                    [ if config.isOn then
                        Css.transform (Css.translateX (Css.px 11))

                      else
                        Css.transform (Css.translateX (Css.px 0))
                    , transition "transform 0.2s ease-in-out"
                    ]
                ]
                [ Svg.circle
                    [ SvgAttributes.cx "15"
                    , SvgAttributes.cy "15"
                    , SvgAttributes.r "14.5"
                    , SvgAttributes.fill "#FFF"
                    , SvgAttributes.css
                        [ if config.isOn then
                            stroke Colors.azure

                          else
                            stroke Colors.gray75
                        , transition "stroke 0.1s"
                        ]
                    , SvgAttributes.class "switch-slider"
                    ]
                    []
                , Svg.path
                    [ SvgAttributes.strokeLinecap "round"
                    , SvgAttributes.strokeLinejoin "round"
                    , SvgAttributes.strokeWidth "3"
                    , SvgAttributes.d "M8 15.865L12.323 20 21.554 10"
                    , SvgAttributes.css
                        [ if config.isOn then
                            stroke Colors.azure

                          else
                            stroke Colors.white
                        , transition "stroke 0.2s"
                        ]
                    ]
                    []
                ]
            ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml


stroke : Color -> Style
stroke color =
    Css.property "stroke" (toCssString color)


transition : String -> Css.Style
transition transitionRules =
    Css.Media.withMediaQuery
        [ "(prefers-reduced-motion: no-preference)" ]
        [ Css.property "transition" transitionRules ]
