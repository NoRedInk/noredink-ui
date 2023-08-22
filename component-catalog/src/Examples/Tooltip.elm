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
import Html.Styled.Attributes exposing (css, href, id)
import KeyboardSupport exposing (Key(..))
import Markdown
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Table.V7 as Table
import Nri.Ui.Tooltip.V4 as Tooltip
import Nri.Ui.UiIcon.V1 as UiIcon


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
    , about = []
    , view = view
    }


type alias State =
    { openTooltip : Maybe TooltipId
    , staticExampleSettings : Control (List ( String, Tooltip.Attribute Never ))
    , pageSettings : Control PageSettings
    }


init : State
init =
    { openTooltip = Nothing
    , staticExampleSettings = initStaticExampleSettings
    , pageSettings =
        Control.record PageSettings
            |> Control.field "backgroundColor"
                (Control.choice
                    [ ( "white", Control.value Colors.white )
                    , ( "azure", Control.value Colors.azure )
                    ]
                )
    }


type alias PageSettings =
    { backgroundColor : Css.Color
    }


type TooltipId
    = PrimaryLabel
    | AuxillaryDescription
    | LearnMore
    | Disclosure


type Msg
    = ToggleTooltip TooltipId Bool
    | SetControl (Control (List ( String, Tooltip.Attribute Never )))
    | UpdatePageSettings (Control PageSettings)
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

        UpdatePageSettings settings ->
            ( { model | pageSettings = settings }, Cmd.none )

        Log message ->
            ( Debug.log "Tooltip Log:" |> always model, Cmd.none )


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig model =
    [ viewCustomizableExample ellieLinkConfig model
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
          , example = viewAuxillaryDescriptionToolip model.openTooltip
          , tooltipId = AuxillaryDescription
          }
        , { name = "Tooltip.disclosure"
          , usage = """
Use when all of the following are true:
- the tooltip trigger only opens the tooltip without doing anything else
- the tooltip trigger ***isn't*** a "?" icon

This type may contain interactive elements such as links.
        """
          , description =
                """
Sometimes a tooltip trigger doesn't have any functionality itself outside of revealing information.

This behavior is analogous to disclosure behavior, except that it's presented different visually. (For more information, please read [Sarah Higley's "Tooltips in the time of WCAG 2.1" post](https://sarahmhigley.com/writing/tooltips-in-wcag-21).)
"""
          , example = viewDisclosureToolip model.openTooltip
          , tooltipId = Disclosure
          }
        , { name = "Tooltip.viewToggleTip"
          , usage = """
Use when all of the following are true:
- the tooltip trigger only opens the tooltip without doing anything else
- the tooltip trigger ***is*** a "?" icon

This type may contain interactive elements such as links.

        """
          , description =
                """
This is a helper for using Tooltip.disclosure with a "?" icon because it is a commonly used UI pattern. We use this helper when we want to show more information about an element but we don't want the element itself to have its own tooltip. The "?" icon typically appears visually adjacent to the element it reveals information about.
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
    Html.span [ css [ Css.displayFlex, Css.alignItems Css.center, Css.justifyContent Css.center ] ]
        [ Html.text "Mastery"
        , Tooltip.viewToggleTip { label = "What is mastery?", lastId = Nothing }
            [ Tooltip.plaintext "Students master topics by correctly answering a series of questions of varying difficulty and scope."
            , Tooltip.onToggle (ToggleTooltip LearnMore)
            , Tooltip.open (openTooltip == Just LearnMore)
            , Tooltip.alignEndForMobile (Css.px 144)
            ]
        ]


initStaticExampleSettings : Control (List ( String, Tooltip.Attribute Never ))
initStaticExampleSettings =
    ControlExtra.list
        |> ControlExtra.listItem "content" controlContent
        |> ControlExtra.optionalListItem "direction" controlDirection
        |> ControlExtra.optionalListItem "direction -- viewport up to 1000px" controlDirectionForMobile
        |> ControlExtra.optionalListItem "direction -- viewport up to 750px" controlDirectionForQuizEngineMobile
        |> ControlExtra.optionalListItem "direction -- viewport up to 500px" controlDirectionForNarrowMobile
        |> ControlExtra.optionalListItem "alignment" controlAlignment
        |> ControlExtra.optionalListItem "alignment -- viewport up to 1000px" controlAlignmentForMobile
        |> ControlExtra.optionalListItem "alignment -- viewport up to 750px" controlAlignmentForQuizEngineMobile
        |> ControlExtra.optionalListItem "alignment -- viewport up to 500px" controlAlignmentForNarrowMobile
        |> ControlExtra.optionalBoolListItem "withoutTail" ( "Tooltip.withoutTail", Tooltip.withoutTail )
        |> ControlExtra.optionalListItem "width" controlWidth
        |> ControlExtra.optionalListItem "padding" controlPadding
        |> CommonControls.css { moduleName = moduleName, use = Tooltip.css }
        |> CommonControls.mobileCss { moduleName = moduleName, use = Tooltip.mobileCss }
        |> CommonControls.quizEngineMobileCss { moduleName = moduleName, use = Tooltip.quizEngineMobileCss }
        |> CommonControls.narrowMobileCss { moduleName = moduleName, use = Tooltip.narrowMobileCss }
        |> CommonControls.notMobileCss { moduleName = moduleName, use = Tooltip.notMobileCss }


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


controlDirectionForQuizEngineMobile : Control ( String, Tooltip.Attribute Never )
controlDirectionForQuizEngineMobile =
    CommonControls.choice "Tooltip"
        [ ( "onTopForQuizEngineMobile", Tooltip.onTopForQuizEngineMobile )
        , ( "onBottomForQuizEngineMobile", Tooltip.onBottomForQuizEngineMobile )
        , ( "onLeftForQuizEngineMobile", Tooltip.onLeftForQuizEngineMobile )
        , ( "onRightForQuizEngineMobile", Tooltip.onRightForQuizEngineMobile )
        ]


controlDirectionForNarrowMobile : Control ( String, Tooltip.Attribute Never )
controlDirectionForNarrowMobile =
    CommonControls.choice "Tooltip"
        [ ( "onTopForNarrowMobile", Tooltip.onTopForNarrowMobile )
        , ( "onBottomForNarrowMobile", Tooltip.onBottomForNarrowMobile )
        , ( "onLeftForNarrowMobile", Tooltip.onLeftForNarrowMobile )
        , ( "onRightForNarrowMobile", Tooltip.onRightForNarrowMobile )
        ]


controlAlignment_ :
    ( String, Tooltip.Attribute Never )
    -> List ( String, Css.Px -> Tooltip.Attribute Never )
    -> Control ( String, Tooltip.Attribute Never )
controlAlignment_ ( middleName, middleValue ) others =
    Control.choice
        (( middleName, Control.value ( "Tooltip." ++ middleName, middleValue ) )
            :: List.map
                (\( name, val ) ->
                    ( name
                    , Control.map
                        (\float ->
                            ( "Tooltip." ++ name ++ " (Css.px " ++ String.fromFloat float ++ ")"
                            , val (Css.px float)
                            )
                        )
                        (ControlExtra.float 0)
                    )
                )
                others
        )


controlAlignment : Control ( String, Tooltip.Attribute Never )
controlAlignment =
    controlAlignment_
        ( "alignMiddle", Tooltip.alignMiddle )
        [ ( "alignStart", Tooltip.alignStart )
        , ( "alignEnd", Tooltip.alignEnd )
        ]


controlAlignmentForMobile : Control ( String, Tooltip.Attribute Never )
controlAlignmentForMobile =
    controlAlignment_
        ( "alignMiddleForMobile", Tooltip.alignMiddleForMobile )
        [ ( "alignStartForMobile", Tooltip.alignStartForMobile )
        , ( "alignEndForMobile", Tooltip.alignEndForMobile )
        ]


controlAlignmentForQuizEngineMobile : Control ( String, Tooltip.Attribute Never )
controlAlignmentForQuizEngineMobile =
    controlAlignment_
        ( "alignMiddleForQuizEngineMobile", Tooltip.alignMiddleForQuizEngineMobile )
        [ ( "alignStartForQuizEngineMobile", Tooltip.alignStartForQuizEngineMobile )
        , ( "alignEndForQuizEngineMobile", Tooltip.alignEndForQuizEngineMobile )
        ]


controlAlignmentForNarrowMobile : Control ( String, Tooltip.Attribute Never )
controlAlignmentForNarrowMobile =
    controlAlignment_
        ( "alignMiddleForNarrowMobile", Tooltip.alignMiddleForNarrowMobile )
        [ ( "alignStartForNarrowMobile", Tooltip.alignStartForNarrowMobile )
        , ( "alignEndForNarrowMobile", Tooltip.alignEndForNarrowMobile )
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
        , Control.view UpdatePageSettings state.pageSettings |> Html.fromUnstyled
        , Html.div
            [ css
                [ Css.displayFlex
                , Css.justifyContent Css.center
                , Css.alignItems Css.center
                , Css.height (Css.px 300)
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
