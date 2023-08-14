module Nri.Ui.Switch.V3 exposing
    ( view
    , Attribute
    , selected
    , containerCss, labelCss, custom, nriDescription, testId
    , onSwitch, disabled
    )

{-|


### Changes from V2:

    - Replace underlying checkbox input with a custom implementation

@docs view


### Attributes

@docs Attribute
@docs selected
@docs containerCss, labelCss, custom, nriDescription, testId
@docs onSwitch, disabled

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Css exposing (Color, Style)
import Css.Global as Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui.Colors.Extra exposing (toCssString)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as Extra
import Nri.Ui.MediaQuery.V1 as MediaQuery
import Nri.Ui.Svg.V1 exposing (Svg)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttributes


{-| -}
type Attribute msg
    = Attribute (Config msg -> Config msg)


{-| What is the status of the Switch, selected or not?
-}
selected : Bool -> Attribute msg
selected isSelected =
    Attribute <| \config -> { config | isSelected = isSelected }


{-| Specify what happens when the switch is toggled.
-}
onSwitch : (Bool -> msg) -> Attribute msg
onSwitch onSwitch_ =
    Attribute <| \config -> { config | onSwitch = Just onSwitch_ }


{-| Explicitly specify that you want this switch to be disabled. If you don't
specify `onSwitch`, this is the default, but it's provided so you don't have
to resort to `filterMap` or similar to build a clean list of attributes.
-}
disabled : Bool -> Attribute msg
disabled isDisabled =
    Attribute <| \config -> { config | isDisabled = isDisabled }


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
    , containerCss : List Style
    , labelCss : List Style
    , isDisabled : Bool
    , isSelected : Bool
    , custom : List (Html.Attribute Never)
    }


defaultConfig : Config msg
defaultConfig =
    { onSwitch = Nothing
    , containerCss = []
    , labelCss = []
    , isDisabled = False
    , isSelected = False
    , custom = []
    }


{-| Render a switch. The boolean here indicates whether the switch is on
or not.
-}
view : { label : String, id : String } -> List (Attribute msg) -> Html msg
view { label, id } attrs =
    let
        config =
            List.foldl (\(Attribute update) -> update) defaultConfig attrs

        isDisabled_ =
            notOperable config
    in
    Html.div
        ([ Attributes.id (id ++ "-container")
         , Attributes.css
            [ Css.display Css.inlineFlex
            , Css.alignItems Css.center
            , Css.fontSize (Css.px 15)
            , Css.outline Css.none
            , Css.pseudoClass "focus-within"
                [ Global.descendants
                    [ Global.class "switch-track"
                        [ FocusRing.boxShadows []
                        , Css.borderRadius (Css.px 16)
                        ]
                    ]
                ]
            , Css.cursor
                (if isDisabled_ then
                    Css.notAllowed

                 else
                    Css.pointer
                )
            , Css.batch config.containerCss
            ]
         , Attributes.class FocusRing.customClass
         ]
            ++ switchAttributes id config
            ++ List.map (Attributes.map never) config.custom
        )
        [ Nri.Ui.Svg.V1.toHtml
            (viewSwitch
                { id = id
                , isSelected = config.isSelected
                , isDisabled = isDisabled_
                }
            )
        , Html.span
            [ Attributes.css
                [ Css.fontWeight (Css.int 600)
                , Css.color
                    (if isDisabled_ then
                        Colors.gray45

                     else
                        Colors.navy
                    )
                , Css.paddingLeft (Css.px 5)
                , Fonts.baseFont
                , Css.batch config.labelCss
                ]
            ]
            [ Html.text label ]
        ]


switchAttributes : String -> Config msg -> List (Html.Attribute msg)
switchAttributes id config =
    let
        eventsOrDisabled =
            case ( config.onSwitch, config.isDisabled ) of
                ( Just onSwitch_, False ) ->
                    [ Events.onClick (onSwitch_ (not config.isSelected))
                    , Key.onKeyDownPreventDefault
                        [ Key.space (onSwitch_ (not config.isSelected))
                        ]
                    ]

                _ ->
                    [ Aria.disabled True ]
    in
    [ Attributes.id id
    , Role.switch
    , Aria.checked (Just config.isSelected)
    , Key.tabbable True
    ]
        ++ eventsOrDisabled


notOperable : Config msg -> Bool
notOperable config =
    config.onSwitch == Nothing || config.isDisabled


viewSwitch :
    { id : String
    , isSelected : Bool
    , isDisabled : Bool
    }
    -> Svg
viewSwitch config =
    let
        shadowFilterId =
            config.id ++ "-shadow-filter"

        shadowBoxId =
            config.id ++ "-shadow-box"

        disabledPrimaryCircleColor =
            if config.isSelected then
                Colors.gray45

            else
                Colors.gray75
    in
    Nri.Ui.Svg.V1.init "0 0 43 32"
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
                        [ if config.isDisabled then
                            if config.isSelected then
                                Css.fill Colors.gray75

                            else
                                Css.fill Colors.gray85

                          else if config.isSelected then
                            Css.fill Colors.glacier

                          else
                            Css.fill Colors.gray92
                        , transition "fill 0.2s"
                        ]
                    ]
                    []
                , if not config.isDisabled then
                    Svg.use
                        [ SvgAttributes.xlinkHref ("#" ++ shadowBoxId)
                        , SvgAttributes.fill "#000"
                        , SvgAttributes.filter ("url(#" ++ shadowFilterId ++ ")")
                        ]
                        []

                  else
                    Svg.g [] []
                ]
            , Svg.g
                [ SvgAttributes.css
                    [ if config.isSelected then
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
                    , SvgAttributes.fill
                        (if config.isDisabled then
                            disabledPrimaryCircleColor

                         else
                            Colors.white
                        ).value
                    , SvgAttributes.css
                        [ if config.isDisabled then
                            stroke disabledPrimaryCircleColor

                          else if config.isSelected then
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
                        [ if config.isDisabled && config.isSelected then
                            stroke Colors.white

                          else if config.isSelected then
                            stroke Colors.azure

                          else
                            Css.display Css.none
                        , transition "stroke 0.2s"
                        ]
                    ]
                    []
                ]
            ]
        ]
        |> Nri.Ui.Svg.V1.withWidth (Css.px 43)
        |> Nri.Ui.Svg.V1.withHeight (Css.px 32)
        |> Nri.Ui.Svg.V1.withCustom [ SvgAttributes.class "switch-track" ]


stroke : Color -> Style
stroke color =
    Css.property "stroke" (toCssString color)


transition : String -> Css.Style
transition transitionRules =
    MediaQuery.anyMotion [ Css.property "transition" transitionRules ]
