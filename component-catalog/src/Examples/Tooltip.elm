module Examples.Tooltip exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Key as Key
import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled as RootHtml
import Html.Styled.Attributes exposing (css, href, id)
import Html.Styled.Events as Events
import Json.Decode as Decode
import KeyboardSupport exposing (Key(..))
import List.Nonempty exposing (Nonempty(..))
import Markdown
import Nri.Ui.Button.V10 as Button
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Table.V9 as Table
import Nri.Ui.Tooltip.V4 as Tooltip
import Nri.Ui.UiIcon.V2 as UiIcon
import Routes
import UsageExamples.ClickableCardWithTooltip


version : Int
version =
    4


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
    , init = ( init, Cmd.none )
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
                , Tooltip.above
                , Tooltip.smallPadding
                , Tooltip.fitToContent
                ]
            ]
        ]
    , about = []
    , view = view
    }


type alias State =
    { openTooltip : Maybe TooltipId
    , staticExampleSettings : Control (List ( String, Tooltip.Attribute Never ))
    , pageSettings : Control PageSettings
    , playground : PlaygroundState
    }


type alias PlaygroundState =
    { isOpen : Bool
    , position : ( Float, Float )
    , dragOffset : Maybe ( Float, Float )
    , preferredSide : PreferredSide
    }


type PreferredSide
    = PreferAbove
    | PreferBelow
    | PreferBefore
    | PreferAfter


init : State
init =
    { openTooltip = Nothing
    , staticExampleSettings = initStaticExampleSettings
    , pageSettings =
        Control.record PageSettings
            |> Control.field "Background color"
                (Control.choice
                    (Nonempty ( "white", Control.value Colors.white )
                        [ ( "azure", Control.value Colors.azure )
                        ]
                    )
                )
    , playground =
        { isOpen = False
        , position = ( 400, 300 )
        , dragOffset = Nothing
        , preferredSide = PreferAbove
        }
    }


type alias PageSettings =
    { backgroundColor : Css.Color
    }


type TooltipId
    = PrimaryLabel
    | AuxillaryDescription
    | HelpfullyDisabled
    | Disclosure


type Msg
    = ToggleTooltip TooltipId Bool
    | SetControl (Control (List ( String, Tooltip.Attribute Never )))
    | UpdatePageSettings (Control PageSettings)
    | Log String
    | OpenPlayground
    | ClosePlayground
    | StartDrag { mouseX : Float, mouseY : Float }
    | DragMove { mouseX : Float, mouseY : Float }
    | EndDrag
    | SetPreferredSide PreferredSide


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

        UpdatePageSettings settings ->
            ( { model | pageSettings = settings }, Cmd.none )

        Log _ ->
            ( model, Cmd.none )

        OpenPlayground ->
            ( { model | playground = setPlaygroundOpen True model.playground }, Cmd.none )

        ClosePlayground ->
            ( { model | playground = setPlaygroundOpen False model.playground }, Cmd.none )

        StartDrag { mouseX, mouseY } ->
            let
                ( px, py ) =
                    model.playground.position

                playground =
                    model.playground
            in
            ( { model
                | playground =
                    { playground
                        | dragOffset = Just ( mouseX - px, mouseY - py )
                    }
              }
            , Cmd.none
            )

        DragMove { mouseX, mouseY } ->
            case model.playground.dragOffset of
                Just ( ox, oy ) ->
                    let
                        playground =
                            model.playground
                    in
                    ( { model
                        | playground =
                            { playground
                                | position = ( mouseX - ox, mouseY - oy )
                            }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        EndDrag ->
            let
                playground =
                    model.playground
            in
            ( { model | playground = { playground | dragOffset = Nothing } }, Cmd.none )

        SetPreferredSide side ->
            let
                playground =
                    model.playground
            in
            ( { model | playground = { playground | preferredSide = side } }, Cmd.none )


setPlaygroundOpen : Bool -> PlaygroundState -> PlaygroundState
setPlaygroundOpen isOpen playground =
    { playground | isOpen = isOpen, dragOffset = Nothing }


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig model =
    [ viewCustomizableExample ellieLinkConfig model
    , viewAutoFlipPlaygroundLauncher
    , viewAutoFlipPlayground model.staticExampleSettings model.playground
    , Heading.h2 [ Heading.plaintext "What type of tooltip should I use?" ]
    , Table.view []
        [ Table.string
            { header = "Type"
            , value = .name
            , width = Css.pct 15
            , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.top, Css.fontWeight Css.bold ]
            , sort = Nothing
            }
        , Table.custom
            { header = Html.text "Usage"
            , view = .usage >> Markdown.toHtml Nothing >> List.map Html.fromUnstyled >> Html.span []
            , width = Css.px 150
            , cellStyles = always [ Css.padding2 Css.zero (Css.px 7), Css.verticalAlign Css.top ]
            , sort = Nothing
            }
        , Table.custom
            { header = Html.text "About"
            , view = .description >> Markdown.toHtml Nothing >> List.map Html.fromUnstyled >> Html.span []
            , width = Css.px 200
            , cellStyles = always [ Css.padding2 Css.zero (Css.px 7), Css.verticalAlign Css.top ]
            , sort = Nothing
            }
        , Table.custom
            { header = Html.text "Example"
            , view = .example
            , width = Css.px 50
            , cellStyles = always [ Css.textAlign Css.center ]
            , sort = Nothing
            }
        ]
        [ { name = "Tooltip.primaryLabel"
          , usage = """
Use when all of the following are true:
- the tooltip trigger does more than just reveal the tooltip content
- the content of the tooltip is the same as the name of the tooltip trigger
- the content of the tooltip doesn't contain interactive elements such as links

Think of this as the "What."
"""
          , description =
                """
This is the default tooltip type.

When using the Tooltip component with the ClickableSvg component, the Tooltip acts as a visible text indicator
of ***what*** the tooltip trigger does. The same text is provided to assitive technology via the ClickableSvg's `name`.
"""
          , example = viewPrimaryLabelTooltip model.openTooltip
          , tooltipId = PrimaryLabel
          }
        , { name = "Tooltip.auxiliaryDescription"
          , usage = """
Use when all of the following are true:
- the tooltip trigger does more than just reveal the tooltip content
- the content of the tooltip provides additional information about the functionality of the tooltip trigger itself
- the content of the tooltip doesn't contain interactive elements such as links

Think of this as the "How."
"""
          , description =
                """
In contrast to Tooltip.primaryLabel, Tooltip.auxiliaryDescription provides information about ***how*** the user should expect the tooltip target to behave when activated.

Examples:
- We might show an icon to indicate that a link opens in a new tab. This icon would have a tooltip to explain ***how*** the link will open.
- On a Quick Write teacher preview, we use Tooltip.auxiliaryDescription on the Save button to let teachers know that the Save button will not actually save in the preview.
"""
          , example = viewAuxillaryDescriptionTooltip model.openTooltip
          , tooltipId = AuxillaryDescription
          }
        , { name = "Tooltip.helpfullyDisabled"
          , usage = """
Use when all of the following are true:
- the tooltip trigger is disabled
- the content of the tooltip provides information explaining why the tooltip trigger is disabled
- the tooltip trigger will become enabled through user interactions
- the content of the tooltip doesn't contain interactive elements, such as links
"""
          , description =
                """
Tooltip.helpfullyDisabled provides information about ***why*** the tooltip trigger is disabled.

Example:
- A tooltip might appear on a disabled button to inform the user that the button will become enabled once they've filled out a required form.
"""
          , example = viewHelpfullyDisabledTooltip model.openTooltip
          , tooltipId = HelpfullyDisabled
          }
        , { name = "Tooltip.disclosure"
          , usage = """
Use when all of the following are true:
- the tooltip trigger only opens the tooltip without doing anything else

This type may contain interactive elements such as links.
        """
          , description =
                [ "Sometimes a tooltip trigger doesn't have any functionality itself outside of revealing information.\n\n"
                , "This behavior is analogous to disclosure behavior, except that it's presented different visually. (For more information, please read [Sarah Higley's \"Tooltips in the time of WCAG 2.1\" post](https://sarahmhigley.com/writing/tooltips-in-wcag-21).)\n\n"
                , "Are you trying to use this tooltip type inside a clickable card? Check out [the Clickable Card with Tooltip example]("
                , Routes.usageExampleHref UsageExamples.ClickableCardWithTooltip.example
                , ")."
                ]
                    |> String.join ""
          , example = viewDisclosureTooltip model.openTooltip
          , tooltipId = Disclosure
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


viewAuxillaryDescriptionTooltip : Maybe TooltipId -> Html Msg
viewAuxillaryDescriptionTooltip openTooltip =
    Tooltip.view
        { id = "tooltip__auxiliaryDescription"
        , trigger =
            \eventHandlers ->
                ClickableText.link "Tooltips & Toggletips"
                    [ ClickableText.custom eventHandlers
                    , ClickableText.small
                    , ClickableText.linkExternal "https://inclusive-components.design/tooltips-toggletips/"
                    ]
        }
        [ Tooltip.plaintext "Opens in a new tab"
        , Tooltip.auxiliaryDescription
        , Tooltip.onToggle (ToggleTooltip AuxillaryDescription)
        , Tooltip.open (openTooltip == Just AuxillaryDescription)
        , Tooltip.smallPadding
        , Tooltip.fitToContent
        , Tooltip.forBreakpoint Tooltip.mobile [ Tooltip.before ]
        ]


viewHelpfullyDisabledTooltip : Maybe TooltipId -> Html Msg
viewHelpfullyDisabledTooltip openTooltip =
    Tooltip.view
        { id = "tooltip__helpfullyDisabled"
        , trigger =
            \attrs ->
                Button.button "Save"
                    [ Button.custom attrs
                    , Button.onClick (Log "")
                    , Button.disabled
                    ]
        }
        [ Tooltip.plaintext "Fill out the required fields before saving."
        , Tooltip.helpfullyDisabled
        , Tooltip.onToggle (ToggleTooltip HelpfullyDisabled)
        , Tooltip.open (openTooltip == Just HelpfullyDisabled)
        , Tooltip.forBreakpoint Tooltip.mobile [ Tooltip.before ]
        ]


viewDisclosureTooltip : Maybe TooltipId -> Html Msg
viewDisclosureTooltip openTooltip =
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
        , Tooltip.forBreakpoint Tooltip.mobile [ Tooltip.alignEnd ]
        ]


initStaticExampleSettings : Control (List ( String, Tooltip.Attribute Never ))
initStaticExampleSettings =
    Control.list
        |> ControlExtra.listItem "content" controlContent
        |> ControlExtra.optionalBoolListItem "withoutTail" ( "Tooltip.withoutTail", Tooltip.withoutTail )
        |> ControlExtra.optionalBoolListItem "noFlip" ( "Tooltip.noFlip", Tooltip.noFlip )
        |> ControlExtra.listItems "Position"
            (Control.list
                |> ControlExtra.optionalListItem "position" controlPosition
                |> ControlExtra.optionalListItem "alignment" controlAlignment
            )
        |> ControlExtra.listItems "Size & Padding"
            (Control.list
                |> ControlExtra.optionalListItem "width" controlWidth
                |> ControlExtra.optionalListItem "padding" controlPadding
            )
        |> ControlExtra.listItems "CSS"
            (Control.list
                |> CommonControls.css { moduleName = moduleName, use = Tooltip.css }
            )


controlContent : Control ( String, Tooltip.Attribute Never )
controlContent =
    CommonControls.content
        { moduleName = "Tooltip"
        , paragraph = Just Tooltip.paragraph
        , plaintext = Tooltip.plaintext
        , markdown = Just Tooltip.markdown
        , html = Tooltip.html
        , httpError = Nothing
        }


controlPosition : Control ( String, Tooltip.Attribute Never )
controlPosition =
    CommonControls.choice "Tooltip"
        (Nonempty ( "above", Tooltip.above )
            [ ( "below", Tooltip.below )
            , ( "before", Tooltip.before )
            , ( "after", Tooltip.after )
            ]
        )


controlAlignment : Control ( String, Tooltip.Attribute Never )
controlAlignment =
    CommonControls.choice "Tooltip"
        (Nonempty ( "alignMiddle", Tooltip.alignMiddle )
            [ ( "alignStart", Tooltip.alignStart )
            , ( "alignEnd", Tooltip.alignEnd )
            ]
        )


controlWidth : Control ( String, Tooltip.Attribute Never )
controlWidth =
    Control.choice
        (Nonempty
            ( "exactWidth (default is 320)"
            , Control.map
                (\int ->
                    ( "Tooltip.exactWidth " ++ String.fromInt int, Tooltip.exactWidth int )
                )
                (Control.int 320)
            )
            [ ( "fitToContent", Control.value ( "Tooltip.fitToContent", Tooltip.fitToContent ) )
            ]
        )


controlPadding : Control ( String, Tooltip.Attribute Never )
controlPadding =
    Control.choice
        (Nonempty ( "normalPadding (default)", Control.value ( "Tooltip.normalPadding", Tooltip.normalPadding ) )
            [ ( "smallPadding", Control.value ( "Tooltip.smallPadding", Tooltip.smallPadding ) )
            , ( "customPadding"
              , Control.map
                    (\float ->
                        ( "Tooltip.customPadding " ++ String.fromFloat float
                        , Tooltip.customPadding float
                        )
                    )
                    (Control.float 0)
              )
            ]
        )


viewCustomizableExample : EllieLink.Config -> State -> Html Msg
viewCustomizableExample ellieLinkConfig ({ staticExampleSettings } as state) =
    let
        pageSettings =
            Control.currentValue state.pageSettings
    in
    Html.div []
        [ ControlView.view
            { ellieLinkConfig = ellieLinkConfig
            , name = moduleName
            , version = version
            , update = SetControl
            , settings = staticExampleSettings
            , mainType = Just "RootHtml.Html msg"
            , extraCode = [ "import Nri.Ui.ClickableSvg.V2 as ClickableSvg" ]
            , renderExample = Code.unstyledView
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
                [ Css.marginTop (Css.px 16)
                , Css.borderRadius (Css.px 12)
                , Css.border3 (Css.px 1) Css.solid Colors.gray85
                , Css.overflow Css.hidden
                , Css.backgroundColor Colors.white
                ]
            ]
            [ Html.div
                [ css
                    [ Css.padding2 (Css.px 8) (Css.px 12)
                    , Css.borderBottom3 (Css.px 1) Css.solid Colors.gray92
                    , Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.justifyContent Css.spaceBetween
                    ]
                ]
                [ Html.span
                    [ css
                        [ Css.fontSize (Css.px 12)
                        , Css.color Colors.gray45
                        , Css.property "text-transform" "uppercase"
                        , Css.property "letter-spacing" "0.5px"
                        , Css.fontWeight Css.bold
                        ]
                    ]
                    [ Html.text "Preview" ]
                , Control.view UpdatePageSettings state.pageSettings
                ]
            , Html.div
                [ css
                    [ Css.displayFlex
                    , Css.justifyContent Css.center
                    , Css.alignItems Css.center
                    , Css.minHeight (Css.px 220)
                    , Css.padding (Css.px 24)
                    , Css.backgroundColor pageSettings.backgroundColor
                    ]
                ]
                [ Tooltip.view
                    { trigger =
                        \eventHandlers ->
                            ClickableSvg.button "Up"
                                UiIcon.arrowTop
                                [ ClickableSvg.custom eventHandlers
                                , ClickableSvg.withBorder
                                ]
                    , id = "an-id-for-the-tooltip"
                    }
                    (Tooltip.open True
                        :: List.map Tuple.second (Control.currentValue staticExampleSettings)
                    )
                    |> Html.map never
                ]
            ]
        ]



-- AUTO-FLIP PLAYGROUND


viewAutoFlipPlaygroundLauncher : Html Msg
viewAutoFlipPlaygroundLauncher =
    Html.div
        [ css
            [ Css.marginTop (Css.px 32)
            , Css.marginBottom (Css.px 32)
            , Css.padding (Css.px 24)
            , Css.borderRadius (Css.px 12)
            , Css.border3 (Css.px 1) Css.solid Colors.gray85
            , Css.backgroundColor Colors.frost
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.spaceBetween
            , Css.property "gap" "24px"
            ]
        ]
        [ Html.div []
            [ Heading.h2
                [ Heading.plaintext "Auto-flip playground"
                , Heading.css [ Css.margin Css.zero, Css.fontSize (Css.px 18) ]
                ]
            , Html.p
                [ css
                    [ Css.marginTop (Css.px 6)
                    , Css.marginBottom Css.zero
                    , Css.color Colors.gray20
                    , Css.maxWidth (Css.px 560)
                    ]
                ]
                [ Html.text "Drag a trigger anywhere on the page to watch the tooltip flip sides and re-anchor its tail when it would otherwise clip the viewport." ]
            ]
        , Button.button "Open playground"
            [ Button.onClick OpenPlayground
            , Button.medium
            , Button.secondary
            ]
        ]


viewAutoFlipPlayground :
    Control (List ( String, Tooltip.Attribute Never ))
    -> PlaygroundState
    -> Html Msg
viewAutoFlipPlayground exampleSettings state =
    if not state.isOpen then
        Html.text ""

    else
        let
            ( px, py ) =
                state.position

            preferredAttr =
                case state.preferredSide of
                    PreferAbove ->
                        Tooltip.above

                    PreferBelow ->
                        Tooltip.below

                    PreferBefore ->
                        Tooltip.before

                    PreferAfter ->
                        Tooltip.after

            inheritedAttrs =
                Control.currentValue exampleSettings
                    |> List.map Tuple.second

            overlayMouseEvents =
                case state.dragOffset of
                    Just _ ->
                        [ onMouseMovePosition DragMove
                        , Events.onMouseUp EndDrag
                        ]

                    Nothing ->
                        []
        in
        RootHtml.div
            ([ css
                [ Css.position Css.fixed
                , Css.top Css.zero
                , Css.left Css.zero
                , Css.right Css.zero
                , Css.bottom Css.zero
                , Css.backgroundColor (Css.rgba 250 250 250 0.98)
                , Css.zIndex (Css.int 9999)
                , Css.property "user-select" "none"
                ]
             ]
                ++ overlayMouseEvents
            )
            [ RootHtml.div
                [ css
                    [ Css.position Css.absolute
                    , Css.top (Css.px 16)
                    , Css.left (Css.px 16)
                    , Css.padding2 (Css.px 12) (Css.px 16)
                    , Css.backgroundColor Colors.white
                    , Css.borderRadius (Css.px 8)
                    , Css.boxShadow5 Css.zero (Css.px 4) (Css.px 16) Css.zero (Css.rgba 0 0 0 0.2)
                    , Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.property "gap" "12px"
                    ]
                ]
                [ RootHtml.text "Preferred side:"
                , viewSideButton state.preferredSide PreferAbove "above"
                , viewSideButton state.preferredSide PreferBelow "below"
                , viewSideButton state.preferredSide PreferBefore "before"
                , viewSideButton state.preferredSide PreferAfter "after"
                , RootHtml.div [ css [ Css.width (Css.px 16) ] ] []
                , Button.button "Close playground"
                    [ Button.onClick ClosePlayground
                    , Button.small
                    , Button.tertiary
                    ]
                ]
            , RootHtml.div
                [ css
                    [ Css.position Css.absolute
                    , Css.left (Css.px px)
                    , Css.top (Css.px py)
                    ]
                , onMouseDownPosition StartDrag
                ]
                [ Tooltip.view
                    { id = "auto-flip-playground-tooltip"
                    , trigger =
                        \attrs ->
                            RootHtml.div
                                (attrs
                                    ++ [ Html.Styled.Attributes.css
                                            [ Css.width (Css.px 96)
                                            , Css.height (Css.px 96)
                                            , Css.borderRadius (Css.pct 50)
                                            , Css.backgroundColor Colors.azure
                                            , Css.color Colors.white
                                            , Css.displayFlex
                                            , Css.alignItems Css.center
                                            , Css.justifyContent Css.center
                                            , Css.fontWeight Css.bold
                                            , Css.cursor Css.move
                                            , Css.boxShadow5 Css.zero (Css.px 2) (Css.px 8) Css.zero (Css.rgba 0 0 0 0.3)
                                            ]
                                       ]
                                )
                                [ RootHtml.text "Drag" ]
                    }
                    (inheritedAttrs
                        ++ [ Tooltip.plaintext
                                ("I prefer to be \""
                                    ++ sideLabel state.preferredSide
                                    ++ "\", but I will flip if I would clip the viewport."
                                )
                           , preferredAttr
                           , Tooltip.open True
                           ]
                    )
                    |> Html.map never
                ]
            ]


viewSideButton : PreferredSide -> PreferredSide -> String -> RootHtml.Html Msg
viewSideButton current target label =
    RootHtml.button
        [ Events.onClick (SetPreferredSide target)
        , css
            [ Css.padding2 (Css.px 4) (Css.px 10)
            , Css.borderRadius (Css.px 4)
            , Css.border3 (Css.px 1)
                Css.solid
                (if current == target then
                    Colors.azure

                 else
                    Colors.gray85
                )
            , Css.backgroundColor
                (if current == target then
                    Colors.frost

                 else
                    Colors.white
                )
            , Css.cursor Css.pointer
            , Css.fontSize (Css.px 13)
            ]
        ]
        [ RootHtml.text label ]


sideLabel : PreferredSide -> String
sideLabel side =
    case side of
        PreferAbove ->
            "above"

        PreferBelow ->
            "below"

        PreferBefore ->
            "before"

        PreferAfter ->
            "after"


onMouseDownPosition : ({ mouseX : Float, mouseY : Float } -> msg) -> RootHtml.Attribute msg
onMouseDownPosition toMsg =
    Events.on "mousedown" (mousePositionDecoder |> Decode.map toMsg)


onMouseMovePosition : ({ mouseX : Float, mouseY : Float } -> msg) -> RootHtml.Attribute msg
onMouseMovePosition toMsg =
    Events.on "mousemove" (mousePositionDecoder |> Decode.map toMsg)


mousePositionDecoder : Decode.Decoder { mouseX : Float, mouseY : Float }
mousePositionDecoder =
    Decode.map2 (\x y -> { mouseX = x, mouseY = y })
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
