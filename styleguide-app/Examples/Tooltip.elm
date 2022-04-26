module Examples.Tooltip exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Key as Key
import Category exposing (Category(..))
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled.Attributes exposing (css, href)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Table.V5 as Table
import Nri.Ui.Text.V6 as Text
import Nri.Ui.Tooltip.V3 as Tooltip
import Nri.Ui.UiIcon.V1 as UiIcon


version : Int
version =
    3


moduleName : String
moduleName =
    "Tooltip"


example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Messaging ]
    , keyboardSupport =
        [ { keys = [ Esc ]
          , result = "Hitting escape while focusing a tooltip trigger closes all tooltips. Note that hovered-but-not-focused tooltips can't be closed this way."
          }
        , { keys = [ Space ]
          , result = "While focusing a tooltip trigger, opens/closes the tooltip. May trigger the underlying action too."
          }
        , { keys = [ Enter ]
          , result = "While focusing a tooltip trigger, opens/closes the tooltip. May trigger the underlying action too."
          }
        ]
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Html.div
            [ css
                [ Css.marginTop (Css.px 60)
                , Css.alignSelf Css.center
                ]
            ]
            [ Tooltip.view
                { id = "preview-tooltip"
                , trigger =
                    \attributes ->
                        ClickableSvg.button "example-preview-tooltip-icon"
                            UiIcon.gear
                            [ ClickableSvg.custom attributes
                            , ClickableSvg.small
                            , ClickableSvg.custom [ Key.tabbable False ]
                            ]
                }
                [ Tooltip.plaintext "This is a tooltip."
                , Tooltip.open True
                , Tooltip.onTop
                , Tooltip.smallPadding
                , Tooltip.fitToContent
                ]
            ]
        ]
    , view = view
    }


type alias State =
    { openTooltip : Maybe TooltipId
    , staticExampleSettings : Control (List ( String, Tooltip.Attribute Never ))
    }


init : State
init =
    { openTooltip = Nothing
    , staticExampleSettings = initStaticExampleSettings
    }


type TooltipId
    = PrimaryLabel
    | AuxillaryDescription
    | LearnMore


type Msg
    = ToggleTooltip TooltipId Bool
    | SetControl (Control (List ( String, Tooltip.Attribute Never )))


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        ToggleTooltip type_ isOpen ->
            if isOpen then
                ( { model | openTooltip = Just type_ }, Cmd.none )

            else
                ( { model | openTooltip = Nothing }, Cmd.none )

        SetControl settings ->
            ( { model | staticExampleSettings = settings }, Cmd.none )


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig model =
    [ viewCustomizableExample ellieLinkConfig model.staticExampleSettings
    , Table.view
        [ Table.string
            { header = "Attribute"
            , value = .name
            , width = Css.px 50
            , cellStyles = always []
            }
        , Table.custom
            { header = Html.text "view"
            , view = .view
            , width = Css.px 50
            , cellStyles = always []
            }
        ]
        [ { name = "Tooltip.primaryLabel"
          , view = viewPrimaryLabelTooltip model.openTooltip
          , tooltipId = PrimaryLabel
          }
        , { name = "Tooltip.auxillaryDescription"
          , view = viewAuxillaryDescriptionToolip model.openTooltip
          , tooltipId = AuxillaryDescription
          }
        , { name = "Tooltip.toggleTip"
          , view = viewToggleTip model.openTooltip
          , tooltipId = LearnMore
          }
        ]
    ]


viewPrimaryLabelTooltip : Maybe TooltipId -> Html Msg
viewPrimaryLabelTooltip openTooltip =
    Tooltip.view
        { id = "tooltip__primaryLabel"
        , trigger =
            \eventHandlers ->
                ClickableSvg.button "Download"
                    UiIcon.download
                    [ ClickableSvg.custom eventHandlers
                    ]
        }
        [ Tooltip.plaintext "Download"
        , Tooltip.primaryLabel
        , Tooltip.onHover (ToggleTooltip PrimaryLabel)
        , Tooltip.open (openTooltip == Just PrimaryLabel)
        , Tooltip.smallPadding
        , Tooltip.fitToContent
        ]


viewAuxillaryDescriptionToolip : Maybe TooltipId -> Html Msg
viewAuxillaryDescriptionToolip openTooltip =
    Tooltip.view
        { id = "tooltip__auxillaryDescription"
        , trigger =
            \eventHandlers ->
                ClickableSvg.button "Period 1"
                    UiIcon.class
                    [ ClickableSvg.custom eventHandlers
                    ]
        }
        [ Tooltip.plaintext "Manage class and students"
        , Tooltip.auxillaryDescription
        , Tooltip.onHover (ToggleTooltip AuxillaryDescription)
        , Tooltip.open (openTooltip == Just AuxillaryDescription)
        , Tooltip.smallPadding
        , Tooltip.fitToContent
        ]


viewToggleTip : Maybe TooltipId -> Html Msg
viewToggleTip openTooltip =
    Tooltip.toggleTip { label = "What is mastery?" }
        [ Tooltip.plaintext "Students master topics by correctly answering a series of questions of varying difficulty and scope."
        , Tooltip.onHover (ToggleTooltip LearnMore)
        , Tooltip.open (openTooltip == Just LearnMore)
        ]


initStaticExampleSettings : Control (List ( String, Tooltip.Attribute Never ))
initStaticExampleSettings =
    ControlExtra.list
        |> ControlExtra.listItem "content" controlContent
        |> ControlExtra.optionalListItem "direction" controlDirection
        |> ControlExtra.optionalListItem "alignment" controlAlignment
        |> ControlExtra.optionalBoolListItem "withoutTail" ( "Tooltip.withoutTail", Tooltip.withoutTail )
        |> ControlExtra.optionalListItem "width" controlWidth
        |> ControlExtra.optionalListItem "padding" controlPadding


controlContent : Control ( String, Tooltip.Attribute Never )
controlContent =
    CommonControls.content
        { moduleName = "Tooltip"
        , plaintext = Tooltip.plaintext
        , markdown = Nothing
        , html = Tooltip.html
        , httpError = Nothing
        }


controlDirection : Control ( String, Tooltip.Attribute Never )
controlDirection =
    CommonControls.choice "Tooltip"
        [ ( "onTop", Tooltip.onTop )
        , ( "onBottom", Tooltip.onBottom )
        , ( "onLeft", Tooltip.onLeft )
        , ( "onRight", Tooltip.onRight )
        ]


controlAlignment : Control ( String, Tooltip.Attribute Never )
controlAlignment =
    Control.choice
        [ ( "alignMiddle (default)", Control.value ( "Tooltip.alignMiddle", Tooltip.alignMiddle ) )
        , ( "alignStart"
          , Control.map
                (\float ->
                    ( "Tooltip.alignStart (Css.px " ++ String.fromFloat float ++ ")"
                    , Tooltip.alignStart (Css.px float)
                    )
                )
                (ControlExtra.float 0)
          )
        , ( "alignEnd"
          , Control.map
                (\float ->
                    ( "Tooltip.alignEnd (Css.px " ++ String.fromFloat float ++ ")"
                    , Tooltip.alignEnd (Css.px float)
                    )
                )
                (ControlExtra.float 0)
          )
        ]


controlWidth : Control ( String, Tooltip.Attribute Never )
controlWidth =
    Control.choice
        [ ( "exactWidth (default is 320)"
          , Control.map
                (\int ->
                    ( "Tooltip.exactWidth " ++ String.fromInt int, Tooltip.exactWidth int )
                )
                (ControlExtra.int 320)
          )
        , ( "fitToContent", Control.value ( "Tooltip.fitToContent", Tooltip.fitToContent ) )
        ]


controlPadding : Control ( String, Tooltip.Attribute Never )
controlPadding =
    Control.choice
        [ ( "normalPadding (default)", Control.value ( "Tooltip.normalPadding", Tooltip.normalPadding ) )
        , ( "smallPadding", Control.value ( "Tooltip.smallPadding", Tooltip.smallPadding ) )
        , ( "customPadding"
          , Control.map
                (\float ->
                    ( "Tooltip.customPadding " ++ String.fromFloat float
                    , Tooltip.customPadding float
                    )
                )
                (ControlExtra.float 0)
          )
        ]


viewCustomizableExample : EllieLink.Config -> Control (List ( String, Tooltip.Attribute Never )) -> Html Msg
viewCustomizableExample ellieLinkConfig controlSettings =
    Html.div []
        [ ControlView.view
            { ellieLinkConfig = ellieLinkConfig
            , name = moduleName
            , version = version
            , update = SetControl
            , settings = controlSettings
            , mainType = "RootHtml.Html msg"
            , extraImports = []
            , toExampleCode =
                \controls ->
                    [ { sectionName = "Example"
                      , code =
                            String.join "\n"
                                [ "Tooltip.view"
                                , "    { trigger ="
                                , "        \\popupTriggerAttributes ->"
                                , "            ClickableSvg.button \"Up\""
                                , "                UiIcon.arrowTop"
                                , "                [ ClickableSvg.custom popupTriggerAttributes"
                                , "                ]"
                                , "    , id = \"an-id-for-the-tooltip\""
                                , "    }"
                                , "    [ "
                                    ++ String.join "\n    , "
                                        ("Tooltip.open True" :: List.map Tuple.first controls)
                                , "    ]"
                                ]
                      }
                    ]
            }
        , Html.div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                , Css.height (Css.px 300)
                ]
            ]
            [ Tooltip.view
                { trigger =
                    \eventHandlers ->
                        ClickableSvg.button "Up"
                            UiIcon.arrowTop
                            [ ClickableSvg.custom eventHandlers
                            ]
                , id = "an-id-for-the-tooltip"
                }
                (Tooltip.open True
                    :: List.map Tuple.second (Control.currentValue controlSettings)
                )
                |> Html.map never
            ]
        ]
