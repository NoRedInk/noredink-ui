module Examples.ClickableSvg exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category exposing (Category(..))
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EventExtras
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.Extra exposing (fromCssColor, toCssColor)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Tooltip.V2 as Tooltip
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
example : Example State Msg
example =
    { name = "ClickableSvg"
    , version = 2
    , categories = [ Buttons, Icons ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ ClickableSvg.link "ClickableSvg small"
            UiIcon.link
            [ ClickableSvg.small
            , ClickableSvg.custom [ Key.tabbable False ]
            ]
        , ClickableSvg.link "ClickableSvg medium"
            UiIcon.link
            [ ClickableSvg.medium
            , ClickableSvg.custom [ Key.tabbable False ]
            ]
        , ClickableSvg.link "ClickableSvg large"
            UiIcon.link
            [ ClickableSvg.large
            , ClickableSvg.custom [ Key.tabbable False ]
            ]
        ]
    , view =
        \state ->
            [ ControlView.view
                { update = SetControls
                , settings = state.settings
                , toExampleCode =
                    \{ label, icon, attributes } ->
                        let
                            toCode fName =
                                "ClickableSvg."
                                    ++ fName
                                    ++ " \""
                                    ++ label
                                    ++ "\"\n\t"
                                    ++ Tuple.first icon
                                    ++ ControlView.codeFromList attributes
                        in
                        [ { sectionName = "Button"
                          , code = toCode "button"
                          }
                        , { sectionName = "Link"
                          , code = toCode "link"
                          }
                        ]
                }
            , viewExampleTable (Control.currentValue state.settings)
            , viewExample
                """
Tooltip.view
    { trigger =
        \\attrs ->
            ClickableSvg.button "Preview"
                UiIcon.preview
                [ ClickableSvg.custom attrs,
                , ClickableSvg.custom [ EventExtras.onClickStopPropagation (ShowItWorked "You clicked the preview button!") ]
                ]
    , id = "preview-tooltip"
    }
    [ Tooltip.plaintext "Preview"
    , Tooltip.primaryLabel
    , Tooltip.onHover SetPreviewTooltip
    , Tooltip.open state.tooltipPreview
    , Tooltip.smallPadding
    , Tooltip.fitToContent
    ]
            """
              <|
                Tooltip.view
                    { trigger =
                        \attrs ->
                            ClickableSvg.button "Preview"
                                UiIcon.preview
                                [ ClickableSvg.custom attrs
                                , ClickableSvg.custom [ EventExtras.onClickStopPropagation (ShowItWorked "You clicked the preview button!") ]
                                ]
                    , id = "preview-tooltip"
                    }
                    [ Tooltip.plaintext "Preview"
                    , Tooltip.primaryLabel
                    , Tooltip.onHover SetPreviewTooltip
                    , Tooltip.open state.tooltipPreview
                    , Tooltip.smallPadding
                    , Tooltip.fitToContent
                    ]
            ]
    }


viewExampleTable : Settings Msg -> Html Msg
viewExampleTable { label, icon, attributes } =
    let
        sharedAttributes =
            List.map Tuple.second attributes

        viewExampleRow index ( themeName, theme ) =
            Html.tr []
                [ cell index [ Html.text themeName ]
                , cell index [ buttonExample (theme :: sharedAttributes) ]
                , cell index [ linkExample (theme :: sharedAttributes) ]
                , cell index [ buttonExample (ClickableSvg.withBorder :: theme :: sharedAttributes) ]
                , cell index [ linkExample (ClickableSvg.withBorder :: theme :: sharedAttributes) ]
                ]

        cell index =
            Html.td
                [ Attributes.css
                    [ if modBy 2 index == 0 then
                        Css.backgroundColor Colors.gray96

                      else
                        Css.backgroundColor Colors.white
                    , Css.padding (Css.px 10)
                    ]
                ]

        buttonExample attributes_ =
            ClickableSvg.button label
                (Tuple.second icon)
                (ClickableSvg.onClick (ShowItWorked "You clicked the back button!")
                    :: attributes_
                )

        linkExample attributes_ =
            ClickableSvg.link label
                (Tuple.second icon)
                (ClickableSvg.linkSpa "some_link" :: attributes_)
    in
    Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "theme" ]
                , Html.th [ Attributes.colspan 2 ] [ Html.text "" ]
                , Html.th [ Attributes.colspan 2 ] [ Html.text "withBorder" ]
                ]
            ]
        , Html.tbody [] <|
            List.indexedMap viewExampleRow
                [ ( "primary", ClickableSvg.primary )
                , ( "secondary", ClickableSvg.secondary )
                , ( "danger", ClickableSvg.danger )
                , ( "dangerSecondary", ClickableSvg.dangerSecondary )
                ]
        , Html.tfoot []
            [ Html.tr []
                [ Html.td [] [ Html.text "" ]
                , Html.td [] [ Html.text "button" ]
                , Html.td [] [ Html.text "link" ]
                , Html.td [] [ Html.text "button" ]
                , Html.td [] [ Html.text "link" ]
                ]
            ]
        ]


viewExample : String -> Html.Html msg -> Html.Html msg
viewExample code html =
    Html.div
        [ Attributes.css [ Css.displayFlex, Css.alignItems Css.center ] ]
        [ html
        , viewCode code
        ]


viewCode : String -> Html.Html msg
viewCode renderStrategy =
    Html.code
        [ Attributes.css
            [ Css.width (Css.px 400)
            , Css.marginLeft (Css.px 20)
            ]
        ]
        [ Html.pre [] [ Html.text renderStrategy ] ]


{-| -}
type alias State =
    { tooltipPreview : Bool
    , tooltipShareTo : Bool
    , settings : Control (Settings Msg)
    }


{-| -}
init : State
init =
    { tooltipPreview = False
    , tooltipShareTo = False
    , settings = initSettings
    }


{-| -}
type Msg
    = ShowItWorked String
    | SetPreviewTooltip Bool
    | SetShareTooltip Bool
    | SetControls (Control (Settings Msg))


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ShowItWorked message ->
            let
                _ =
                    Debug.log "ClickableSvg" message
            in
            ( state, Cmd.none )

        SetPreviewTooltip bool ->
            ( { state | tooltipPreview = bool }, Cmd.none )

        SetShareTooltip bool ->
            ( { state | tooltipShareTo = bool }, Cmd.none )

        SetControls settings ->
            ( { state | settings = settings }, Cmd.none )


type alias Settings msg =
    { label : String
    , icon : ( String, Svg )
    , attributes : List ( String, ClickableSvg.Attribute msg )
    }


initSettings : Control (Settings msg)
initSettings =
    Control.record Settings
        |> Control.field "label" (Control.string "Back")
        |> Control.field "icon" CommonControls.uiIcon
        |> Control.field "attributes"
            (ControlExtra.list
                |> CommonControls.disabledListItem "ClickableSvg" ClickableSvg.disabled
                |> ControlExtra.optionalListItem "exactSize"
                    (Control.map
                        (\v -> ( "ClickableSvg.exactSize " ++ String.fromInt v, ClickableSvg.exactSize v ))
                        (ControlExtra.int 36)
                    )
                |> CommonControls.css
                    { moduleName = "ClickableSvg"
                    , use = ClickableSvg.css
                    , default = "border: 2px solid red;"
                    }
                |> CommonControls.mobileCss
                    { moduleName = "ClickableSvg"
                    , use = ClickableSvg.mobileCss
                    , default = "padding: 10px;"
                    }
                |> CommonControls.quizEngineMobileCss
                    { moduleName = "ClickableSvg"
                    , use = ClickableSvg.quizEngineMobileCss
                    , default = ""
                    }
                |> CommonControls.notMobileCss
                    { moduleName = "ClickableSvg"
                    , use = ClickableSvg.notMobileCss
                    , default = ""
                    }
            )
