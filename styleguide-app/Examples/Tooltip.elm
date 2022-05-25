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
import Html.Styled.Attributes exposing (css, href, id)
import KeyboardSupport exposing (Key(..))
import Markdown
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Table.V5 as Table
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
    | Disclosure


type Msg
    = ToggleTooltip TooltipId Bool
    | SetControl (Control (List ( String, Tooltip.Attribute Never )))
    | Log String


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

        Log message ->
            ( Debug.log "Tooltip Log:" |> always model, Cmd.none )


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig model =
    [ viewCustomizableExample ellieLinkConfig model.staticExampleSettings
    , Table.view
        [ Table.string
            { header = "Type"
            , value = .name
            , width = Css.pct 15
            , cellStyles = always [Css.padding2 Css.zero (Css.px 7)]
            }
        , Table.custom
            { header = Html.text "Usage"
            , view = .usage >> Markdown.toHtml Nothing >> List.map Html.fromUnstyled >> Html.span []
            , width = Css.px 150
            , cellStyles = always [Css.padding2 Css.zero (Css.px 7)]
            }
        , Table.custom
            { header = Html.text "About"
            , view = .description >> Markdown.toHtml Nothing >> List.map Html.fromUnstyled >> Html.span []
            , width = Css.px 200
            , cellStyles = always [Css.padding2 Css.zero (Css.px 7)]
            }
        , Table.custom
            { header = Html.text "Example"
            , view = .example
            , width = Css.px 50
            , cellStyles = always [ Css.textAlign Css.center ]
            }
        ]
        [ { name = "Tooltip.primaryLabel"
          , usage = """
Use when all of the following are true:
- the tooltip trigger does more than just reveal the tooltip content
- the content of the tooltip is the same as the name of the tooltip trigger

Think of this as the \"What.\"
"""
          , description =
                """
Used when the content of the tooltip is identical to the accessible name.

For example, when using the Tooltip component with the ClickableSvg component, the Tooltip is providing
extra information to sighted users that screenreader users already have.

This is the default.
"""
          , example = viewPrimaryLabelTooltip model.openTooltip
          , tooltipId = PrimaryLabel
          }
        , { name = "Tooltip.auxiliaryDescription"
        , usage = """
Use when all of the following are true:
- the tooltip trigger does more than just reveal the tooltip content
- the content of the tooltip provides additional information about the functionality of the tooltip trigger itself.

Think of this as the \"How.\"
"""
          , description =
                """
Used when the content of the tooltip provides an "auxiliary description" for its content.

An auxiliary description is used when the tooltip content provides supplementary information about its trigger content.
"""
          , example = viewAuxillaryDescriptionToolip model.openTooltip
          , tooltipId = AuxillaryDescription
          }
        , { name = "Tooltip.disclosure"
        , usage = """
Use when all of the following are true:
- the tooltip trigger only opens the tooltip without doing anything else
- the tooltip trigger ***isn't*** a \"?\" icon
        """
          , description =
                """
Sometimes a "tooltip" only _looks_ like a tooltip, but is really more about hiding and showing extra information when the user asks for it.

If clicking the "tooltip trigger" only ever shows you more info (and especially if this info is rich or interactable), use this attribute.

For more information, please read [Sarah Higley's "Tooltips in the time of WCAG 2.1" post](https://sarahmhigley.com/writing/tooltips-in-wcag-21).
"""
          , example = viewDisclosureToolip model.openTooltip
          , tooltipId = Disclosure
          }
        , { name = "Tooltip.viewToggleTip"
        , usage = """
Use when all of the following are true:
- the tooltip trigger only opens the tooltip without doing anything else
- the tooltip trigger ***is*** a \"?\" icon
        """
          , description =
                """
Supplementary information triggered by a "?" icon.

This is a helper for setting up a commonly-used `disclosure` tooltip. Please see the documentation for `disclosure` to learn more.
"""
          , example = viewToggleTip model.openTooltip
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
                    , ClickableSvg.onClick (Log "Fake content totally downloaded!")
                    ]
        }
        [ Tooltip.plaintext "Download"
        , Tooltip.primaryLabel
        , Tooltip.onToggle (ToggleTooltip PrimaryLabel)
        , Tooltip.open (openTooltip == Just PrimaryLabel)
        , Tooltip.smallPadding
        , Tooltip.fitToContent
        ]


viewAuxillaryDescriptionToolip : Maybe TooltipId -> Html Msg
viewAuxillaryDescriptionToolip openTooltip =
    Tooltip.view
        { id = "tooltip__auxiliaryDescription"
        , trigger =
            \eventHandlers ->
                ClickableSvg.button "Period 1"
                    UiIcon.class
                    [ ClickableSvg.custom eventHandlers
                    , ClickableSvg.onClick (Log "You totally started managing Periud 1.")
                    ]
        }
        [ Tooltip.plaintext "Manage class and students"
        , Tooltip.auxiliaryDescription
        , Tooltip.onToggle (ToggleTooltip AuxillaryDescription)
        , Tooltip.open (openTooltip == Just AuxillaryDescription)
        , Tooltip.smallPadding
        , Tooltip.fitToContent
        , Tooltip.onLeftForMobile
        ]


viewDisclosureToolip : Maybe TooltipId -> Html Msg
viewDisclosureToolip openTooltip =
    let
        triggerId =
            "tooltip__disclosure-trigger"

        lastId =
            "tooltip__disclosure-what-is-mastery"
    in
    Tooltip.view
        { id = "tooltip__disclosure"
        , trigger =
            \eventHandlers ->
                ClickableSvg.button "Previously mastered"
                    (Svg.withColor Colors.green UiIcon.starFilled)
                    [ ClickableSvg.custom eventHandlers
                    , ClickableSvg.id triggerId
                    ]
        }
        [ Tooltip.html
            [ Html.text "You mastered this skill in a previous year! Way to go! "
            , Html.a
                [ id lastId
                , href "https://noredink.zendesk.com/hc/en-us/articles/203022319-What-is-mastery-"
                ]
                [ Html.text "Learn more about NoRedInk Mastery" ]
            ]
        , Tooltip.disclosure { triggerId = triggerId, lastId = Just lastId }
        , Tooltip.onToggle (ToggleTooltip Disclosure)
        , Tooltip.open (openTooltip == Just Disclosure)
        , Tooltip.smallPadding
        , Tooltip.alignEndForMobile (Css.px 148)
        ]


viewToggleTip : Maybe TooltipId -> Html Msg
viewToggleTip openTooltip =
    Tooltip.viewToggleTip { label = "What is mastery?", lastId = Nothing }
        [ Tooltip.plaintext "Students master topics by correctly answering a series of questions of varying difficulty and scope."
        , Tooltip.onToggle (ToggleTooltip LearnMore)
        , Tooltip.open (openTooltip == Just LearnMore)
        , Tooltip.alignEndForMobile (Css.px 144)
        ]


initStaticExampleSettings : Control (List ( String, Tooltip.Attribute Never ))
initStaticExampleSettings =
    ControlExtra.list
        |> ControlExtra.listItem "content" controlContent
        |> ControlExtra.optionalListItem "direction" controlDirection
        |> ControlExtra.optionalListItem "direction -- mobile" controlDirectionForMobile
        |> ControlExtra.optionalListItem "alignment" controlAlignment
        |> ControlExtra.optionalListItem "alignment -- mobile" controlAlignmentForMobile
        |> ControlExtra.optionalBoolListItem "withoutTail" ( "Tooltip.withoutTail", Tooltip.withoutTail )
        |> ControlExtra.optionalListItem "width" controlWidth
        |> ControlExtra.optionalListItem "padding" controlPadding
        |> CommonControls.css { moduleName = moduleName, use = Tooltip.css }
        |> CommonControls.mobileCss { moduleName = moduleName, use = Tooltip.mobileCss }
        |> CommonControls.quizEngineMobileCss { moduleName = moduleName, use = Tooltip.quizEngineMobileCss }
        |> CommonControls.notMobileCss { moduleName = moduleName, use = Tooltip.notMobileCss }


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


controlDirectionForMobile : Control ( String, Tooltip.Attribute Never )
controlDirectionForMobile =
    CommonControls.choice "Tooltip"
        [ ( "onTopForMobile", Tooltip.onTopForMobile )
        , ( "onBottomForMobile", Tooltip.onBottomForMobile )
        , ( "onLeftForMobile", Tooltip.onLeftForMobile )
        , ( "onRightForMobile", Tooltip.onRightForMobile )
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


controlAlignmentForMobile : Control ( String, Tooltip.Attribute Never )
controlAlignmentForMobile =
    Control.choice
        [ ( "alignMiddleForMobile (default)", Control.value ( "Tooltip.alignMiddleForMobile", Tooltip.alignMiddleForMobile ) )
        , ( "alignStartForMobile"
          , Control.map
                (\float ->
                    ( "Tooltip.alignStartForMobile (Css.px " ++ String.fromFloat float ++ ")"
                    , Tooltip.alignStartForMobile (Css.px float)
                    )
                )
                (ControlExtra.float 0)
          )
        , ( "alignEndForMobile"
          , Control.map
                (\float ->
                    ( "Tooltip.alignEndForMobile (Css.px " ++ String.fromFloat float ++ ")"
                    , Tooltip.alignEndForMobile (Css.px float)
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
