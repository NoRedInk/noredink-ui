module Nri.Ui.Switch.V1 exposing (view, Attribute, onSwitch, disabled, id, label, custom)

{-|

@docs view, Attribute, onSwitch, disabled, id, label, custom

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Aria as Aria
import Css
import Css.Global as Global
import Css.Media
import Html.Styled as WildWildHtml
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Svg.V1 exposing (Svg)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttributes


{-| -}
type Attribute msg
    = OnSwitch (Bool -> msg)
    | Id String
    | Label (Html msg)
    | Disabled
    | Custom (List (Html.Attribute Never))


{-| Specify what happens when the switch is toggled.
-}
onSwitch : (Bool -> msg) -> Attribute msg
onSwitch =
    OnSwitch


{-| Explicitly specify that you want this switch to be disabled. If you don't
specify `onSwitch`, this is the default, but it's provided so you don't have
to resort to `filterMap` or similar to build a clean list of attributes.
-}
disabled : Attribute msg
disabled =
    Disabled


{-| Set the HTML ID of the switch toggle. If you have only one on the page,
you don't need to set this, but you should definitely set it if you have
more than one.
-}
id : String -> Attribute msg
id =
    Id


{-| Add labeling text to the switch. This text should be descriptive and
able to be displayed inline. It should _not_ be interactive (if it were
ergonomic to make this argument `Html Never`, we would!)
-}
label : Html msg -> Attribute msg
label =
    Label


{-| Pass custom attributes through to be attached to the underlying input.
-}
custom : List (Html.Attribute Never) -> Attribute msg
custom =
    Custom


type alias Config msg =
    { onSwitch : Maybe (Bool -> msg)
    , id : String
    , label : Maybe (Html msg)
    , attributes : List (Html.Attribute Never)
    }


defaultConfig : Config msg
defaultConfig =
    { onSwitch = Nothing
    , id = "nri-ui-switch-with-default-id"
    , label = Nothing
    , attributes = []
    }


customize : Attribute msg -> Config msg -> Config msg
customize attr config =
    case attr of
        OnSwitch onSwitch_ ->
            { config | onSwitch = Just onSwitch_ }

        Disabled ->
            { config | onSwitch = Nothing }

        Id id_ ->
            { config | id = id_ }

        Label label_ ->
            { config | label = Just label_ }

        Custom custom_ ->
            { config | attributes = custom_ }


{-| Render a switch. The boolean here indicates whether the switch is on
or not.
-}
view : List (Attribute msg) -> Bool -> Html msg
view attrs isOn =
    let
        config =
            List.foldl customize defaultConfig attrs
    in
    WildWildHtml.label
        [ Attributes.id (config.id ++ "-container")
        , Attributes.css
            [ Css.display Css.inlineFlex
            , Css.alignItems Css.center
            , Css.position Css.relative
            , Css.pseudoClass "focus-within"
                [ Global.descendants
                    [ Global.class "switch-slider"
                        [ -- azure, but can't use the Color type here
                          Css.property "stroke" "#146AFF"
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
            ]
        , Aria.controls [ config.id ]
        , Aria.checked (Just isOn)
        ]
        [ viewCheckbox
            { id = config.id
            , onCheck = config.onSwitch
            , checked = isOn
            , attributes = config.attributes
            }
        , Nri.Ui.Svg.V1.toHtml
            (viewSwitch
                { id = config.id
                , isOn = isOn
                , enabled = config.onSwitch /= Nothing
                }
            )
        , case config.label of
            Just label_ ->
                Html.span
                    [ Attributes.css
                        [ Css.fontWeight (Css.int 600)
                        , Css.color Colors.navy
                        , Css.paddingLeft (Css.px 5)
                        ]
                    , Attributes.for config.id
                    ]
                    [ label_ ]

            Nothing ->
                Html.text ""
        ]


viewCheckbox :
    { id : String
    , onCheck : Maybe (Bool -> msg)
    , checked : Bool
    , attributes : List (Html.Attribute Never)
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
                Aria.disabled True
         ]
            ++ List.map (Attributes.map never) config.attributes
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
                            -- azure, but can't use the Color type here
                            Css.property "stroke" "#146AFF"

                          else
                            -- gray75, but can't use the Color type here
                            Css.property "stroke" "#BFBFBF"
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
                            -- azure, but can't use the Color type here
                            Css.property "stroke" "#146AFF"

                          else
                            Css.property "stroke" "rgba(255,255,255,0)"
                        , transition "stroke 0.2s"
                        ]
                    ]
                    []
                ]
            ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml


transition : String -> Css.Style
transition transitionRules =
    Css.Media.withMediaQuery
        [ "(prefers-reduced-motion: no-preference)" ]
        [ Css.property "transition" transitionRules ]
