module Nri.Ui.Switch.V1 exposing (view, Attribute, onSwitch, disabled, id, label)

{-|

@docs view, Attribute, onSwitch, disabled, id, label

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Widget as Widget
import Css
import Css.Global as Global
import Html.Styled as WildWildHtml
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Svg.V1 exposing (Svg)
import Svg.Styled as Svg
import Svg.Styled.Attributes as SvgAttributes


type Attribute msg
    = OnSwitch (Bool -> msg)
    | Id String
    | Label (Html msg)
    | Disabled


onSwitch : (Bool -> msg) -> Attribute msg
onSwitch =
    OnSwitch


{-| note that this will be the default
-}
disabled : Attribute msg
disabled =
    Disabled


id : String -> Attribute msg
id =
    Id


{-| Labeling text requirements: should be descriptive but not interactive
(that is, this API would be `Html Never` if it was ergonomic to do so)
and should be styled so that it can be displayed inline.
-}
label : Html msg -> Attribute msg
label =
    Label


type alias Config msg =
    { onSwitch : Maybe (Bool -> msg)
    , id : String
    , label : Maybe (Html msg)
    }


defaultConfig : Config msg
defaultConfig =
    { onSwitch = Nothing
    , id = "nri-ui-switch-with-default-id"
    , label = Nothing
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


view : List (Attribute msg) -> Bool -> Html msg
view attrs isOn =
    let
        config =
            List.foldl customize defaultConfig attrs
    in
    WildWildHtml.label
        [ Attributes.id (config.id ++ "-container")
        , Attributes.css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.position Css.relative
            , Css.pseudoClass "focus-within"
                [ Global.descendants
                    [ Global.svg [ Css.borderColor Colors.azure ] ]
                ]
            ]
        , Aria.controls config.id
        , Widget.checked (Just isOn)
        ]
        [ viewCheckbox
            { id = config.id
            , onCheck = config.onSwitch
            , checked = isOn
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
                    [ Attributes.css [ Css.paddingLeft (Css.px 5) ] ]
                    [ label_ ]

            Nothing ->
                Html.text ""
        ]


viewCheckbox :
    { id : String
    , onCheck : Maybe (Bool -> msg)
    , checked : Bool
    }
    -> Html msg
viewCheckbox config =
    Html.checkbox config.id
        (Just config.checked)
        [ Attributes.id config.id
        , Attributes.css
            [ Css.position Css.absolute
            , Css.top (Css.px 10)
            , Css.left (Css.px 10)
            , Css.zIndex (Css.int 0)
            ]
        , case config.onCheck of
            Just onCheck ->
                Events.onCheck onCheck

            Nothing ->
                Widget.disabled True
        ]


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
        [ SvgAttributes.width "40"
        , SvgAttributes.height "30"
        , SvgAttributes.viewBox "0 0 41 30"
        , SvgAttributes.css
            [ if config.enabled then
                Css.cursor Css.pointer

              else
                Css.batch []
            , Css.zIndex (Css.int 1)
            , Css.border3 (Css.px 2) Css.solid Css.transparent
            , Css.padding (Css.px 3)
            , Css.borderRadius (Css.px 21)
            ]
        ]
        [ Svg.defs []
            [ Svg.filter
                [ SvgAttributes.id shadowFilterId
                , SvgAttributes.width "105%"
                , SvgAttributes.height "106.7%"
                , SvgAttributes.x "-2.5%"
                , SvgAttributes.y "-3.3%"
                , SvgAttributes.filterUnits "objectboundingBox"
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
            ]
            [ Svg.g []
                [ Svg.use
                    [ SvgAttributes.xlinkHref ("#" ++ shadowBoxId)
                    , SvgAttributes.css
                        [ if config.isOn then
                            Css.fill Colors.glacier

                          else
                            Css.fill Colors.gray92
                        , Css.property "transition" "fill 0.4s"
                        ]
                    ]
                    []
                , Svg.use
                    [ SvgAttributes.xlinkHref ("#" ++ shadowBoxId)
                    , SvgAttributes.fill "#000"
                    , SvgAttributes.filter ("url(" ++ shadowFilterId ++ ")")
                    ]
                    []
                ]
            , Svg.g
                [ SvgAttributes.css
                    [ if config.isOn then
                        Css.transform (Css.translateX (Css.px 11))

                      else
                        Css.batch []
                    , Css.property "transition" "transform 0.4s"
                    ]
                ]
                [ -- <circle cx="15" cy="15" r="14.5" fill="#FFF"/>
                  Svg.circle
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
                            Css.property "stroke" "#EBEBEB"
                        , Css.property "transition" "stroke 0.4s"
                        ]
                    ]
                    []

                -- <path stroke-linecap="round" stroke-linejoin="round" stroke-width="3" d="M8 15.865L12.323 20 21.554 10"/>
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
                            -- gray75, but can't use the Color type here
                            Css.property "stroke" "rgba(255,255,255,0)"
                        , Css.property "transition" "stroke 0.4s"
                        ]
                    ]
                    []
                ]
            ]
        ]
        |> Nri.Ui.Svg.V1.fromHtml
