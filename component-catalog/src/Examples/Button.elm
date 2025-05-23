module Examples.Button exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Category exposing (Category(..))
import Code
import CommonControls
import Css exposing (middle, verticalAlign)
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Guidance
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Text.V6 as Text
import Nri.Ui.Tooltip.V3 as Tooltip
import Nri.Ui.UiIcon.V2 as UiIcon
import Set exposing (Set)


version : Int
version =
    10


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = ( init, Cmd.none )
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ Button.link "Primary"
            [ Button.small
            , Button.fillContainerWidth
            , Button.custom [ Key.tabbable False ]
            , Button.icon UiIcon.link
            ]
        , Button.link "Secondary"
            [ Button.small
            , Button.fillContainerWidth
            , Button.secondary
            , Button.css [ Css.marginTop (Css.px 8) ]
            , Button.custom [ Key.tabbable False ]
            , Button.icon UiIcon.link
            ]
        , Button.link "Tertiary"
            [ Button.small
            , Button.fillContainerWidth
            , Button.tertiary
            , Button.css [ Css.marginTop (Css.px 8) ]
            , Button.custom [ Key.tabbable False ]
            , Button.icon UiIcon.link
            ]
        , Button.link "Premium"
            [ Button.small
            , Button.fillContainerWidth
            , Button.premium
            , Button.css [ Css.marginTop (Css.px 8) ]
            , Button.custom [ Key.tabbable False ]
            , Button.icon UiIcon.link
            ]
        ]
    , about =
        [ Guidance.helpfullyDisabled moduleName
        , Text.smallBody
            [ Text.html
                [ text "Which Button type should you use?"
                ]
            ]
        , ul [ css [ Css.paddingLeft (Css.px 42), Css.margin Css.zero ] ]
            [ li []
                [ Text.smallBody
                    [ Text.html
                        [ strong [] [ text "Link type:" ]
                        , text " When the button takes the user to a URL, whether this results in a new page load or whether the URL is a SPA route. (This allows users to do things like copy the URL, open the link in a new tab, etc.  Please use the "
                        , ClickableText.link "Assistive technology notification design & development guide"
                            [ ClickableText.linkExternal "https://noredinkaccessibility.screenstepslive.com/a/1651037-assistive-technology-notification-design-development-guide"
                            , ClickableText.appearsInline
                            ]
                        , text " to ensure you're managing the user's focus properly within a SPA.)"
                        ]
                    ]
                ]
            , li [] [ Text.smallBody [ Text.markdown "**Button type:** When the button performs an action on the same page, or when the button submits a form, even if the form submission ultimately directs the user to a new URL." ] ]
            ]
        , Guidance.useRadioButtonDotless
        ]
    , view = \ellieLinkConfig state -> [ viewButtonExamples ellieLinkConfig state ]
    , categories = [ Buttons ]
    , keyboardSupport = []
    }


moduleName : String
moduleName =
    "Button"


{-| -}
type alias State =
    { debugControlsState : Control Model
    , pressedToggleButtons : Set Int
    }


{-| -}
init : State
init =
    { debugControlsState = initDebugControls
    , pressedToggleButtons = Set.singleton 1
    }


{-| -}
type ButtonType
    = Button
    | Link


{-| -}
type Msg
    = SetDebugControlsState (Control Model)
    | ShowItWorked String String
    | ToggleToggleButton Int


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetDebugControlsState newDebugControlsState ->
            ( { state | debugControlsState = newDebugControlsState }
            , Cmd.none
            )

        ShowItWorked group message ->
            ( Debug.log group message |> always state, Cmd.none )

        ToggleToggleButton id ->
            ( { state
                | pressedToggleButtons =
                    if Set.member id state.pressedToggleButtons then
                        Set.remove id state.pressedToggleButtons

                    else
                        Set.insert id state.pressedToggleButtons
              }
            , Cmd.none
            )



-- INTERNAL


type alias Model =
    { buttonType : ButtonType
    , label : String
    , attributes : List ( String, Button.Attribute Msg )
    }


{-| -}
initDebugControls : Control Model
initDebugControls =
    Control.record Model
        |> Control.field "type"
            (Control.choice
                [ ( "button", Control.value Button )
                , ( "link", Control.value Link )
                ]
            )
        |> Control.field "label" (Control.string "Label **bold**   *emphasis*")
        |> Control.field "" controlAttributes


controlAttributes : Control (List ( String, Button.Attribute Msg ))
controlAttributes =
    Control.list
        |> ControlExtra.listItems "Icons"
            (Control.list
                |> CommonControls.icon moduleName Button.icon
                |> CommonControls.rightIcon moduleName Button.rightIcon
                |> ControlExtra.optionalBoolListItem "hideIconForMobile"
                    ( "Button.hideIconForMobile", Button.hideIconForMobile )
            )
        |> ControlExtra.listItems "Size & Width"
            (Control.list
                |> ControlExtra.optionalListItem "size"
                    (CommonControls.choice moduleName sizes)
                |> ControlExtra.optionalListItem "width"
                    (CommonControls.choice moduleName
                        [ ( "exactWidth 120", Button.exactWidth 120 )
                        , ( "exactWidth 70", Button.exactWidth 70 )
                        , ( "boundedWidth 100 180", Button.boundedWidth { min = 100, max = 180 } )
                        , ( "unboundedWidth", Button.unboundedWidth )
                        , ( "fillContainerWidth", Button.fillContainerWidth )
                        ]
                    )
                |> ControlExtra.optionalListItem "mobile width"
                    (CommonControls.choice moduleName
                        [ ( "exactWidthForMobile 120", Button.exactWidthForMobile 120 )
                        , ( "exactWidthForMobile 70", Button.exactWidthForMobile 70 )
                        , ( "boundedWidthForMobile 100 180", Button.boundedWidthForMobile { min = 100, max = 180 } )
                        , ( "unboundedWidthForMobile", Button.unboundedWidthForMobile )
                        , ( "fillContainerWidthForMobile", Button.fillContainerWidthForMobile )
                        ]
                    )
                |> ControlExtra.optionalListItem "quiz engine mobile width"
                    (CommonControls.choice moduleName
                        [ ( "exactWidthForQuizEngineMobile 120", Button.exactWidthForQuizEngineMobile 120 )
                        , ( "exactWidthForQuizEngineMobile 70", Button.exactWidthForQuizEngineMobile 70 )
                        , ( "boundedWidthForQuizEngineMobile 100 180", Button.boundedWidthForQuizEngineMobile { min = 100, max = 180 } )
                        , ( "unboundedWidthForQuizEngineMobile", Button.unboundedWidthForQuizEngineMobile )
                        , ( "fillContainerWidthForQuizEngineMobile", Button.fillContainerWidthForQuizEngineMobile )
                        ]
                    )
                |> ControlExtra.optionalListItem "narrow mobile width"
                    (CommonControls.choice moduleName
                        [ ( "exactWidthForNarrowMobile 120", Button.exactWidthForNarrowMobile 120 )
                        , ( "exactWidthForNarrowMobile 70", Button.exactWidthForNarrowMobile 70 )
                        , ( "boundedWidthForNarrowMobile 100 180", Button.boundedWidthForNarrowMobile { min = 100, max = 180 } )
                        , ( "unboundedWidthForNarrowMobile", Button.unboundedWidthForNarrowMobile )
                        , ( "fillContainerWidthForNarrowMobile", Button.fillContainerWidthForNarrowMobile )
                        ]
                    )
            )
        |> ControlExtra.listItems "State & Type"
            (Control.list
                |> ControlExtra.optionalBoolListItem "disabled" ( "disabled", Button.disabled )
                |> ControlExtra.optionalListItem "state (button only)"
                    (CommonControls.choice moduleName
                        [ ( "error", Button.error )
                        , ( "unfulfilled", Button.unfulfilled )
                        , ( "loading", Button.loading )
                        , ( "success", Button.success )
                        ]
                    )
                |> ControlExtra.optionalBoolListItem "toggleButtonPressed"
                    ( "toggleButtonPressed True"
                    , Button.toggleButtonPressed True
                    )
                |> ControlExtra.optionalBoolListItem "submit (button only)"
                    ( "Button.submit", Button.submit )
                |> ControlExtra.optionalBoolListItem "opensModal (button only)"
                    ( "Button.opensModal", Button.opensModal )
            )
        |> ControlExtra.listItems "Theme & CSS"
            (Control.list
                |> ControlExtra.optionalListItem "theme"
                    (CommonControls.choice moduleName
                        [ ( "primary", Button.primary )
                        , ( "secondary", Button.secondary )
                        , ( "tertiary", Button.tertiary )
                        , ( "danger", Button.danger )
                        , ( "dangerSecondary", Button.dangerSecondary )
                        , ( "premium", Button.premium )
                        ]
                    )
                |> CommonControls.css
                    { moduleName = moduleName
                    , use = Button.css
                    }
                |> CommonControls.mobileCss
                    { moduleName = moduleName
                    , use = Button.mobileCss
                    }
                |> CommonControls.quizEngineMobileCss
                    { moduleName = moduleName
                    , use = Button.quizEngineMobileCss
                    }
                |> CommonControls.notMobileCss
                    { moduleName = moduleName
                    , use = Button.notMobileCss
                    }
            )


viewButtonExamples : EllieLink.Config -> State -> Html Msg
viewButtonExamples ellieLinkConfig state =
    let
        model =
            Control.currentValue state.debugControlsState
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = SetDebugControlsState
        , settings = state.debugControlsState
        , mainType = Just "RootHtml.Html msg"
        , extraCode = []
        , renderExample = Code.unstyledView
        , toExampleCode =
            \{ label, attributes } ->
                let
                    toCode fName =
                        Code.fromModule moduleName fName
                            ++ " "
                            ++ Code.string label
                            ++ Code.listMultiline (List.map Tuple.first attributes) 1
                in
                [ { sectionName = "Button"
                  , code = toCode "button"
                  }
                , { sectionName = "Link"
                  , code = toCode "link"
                  }
                ]
        }
    , Heading.h2
        [ Heading.plaintext "Customizable example"
        , Heading.css
            [ Css.marginTop Spacing.verticalSpacerPx
            , Css.marginBottom (Css.px 10)
            ]
        ]
    , viewCustomizableExample model
    , Heading.h2
        [ Heading.plaintext "Non-interactive examples"
        , Heading.css
            [ Css.marginTop Spacing.verticalSpacerPx
            , Css.marginBottom (Css.px 10)
            ]
        ]
    , buttonsTable
    , toggleButtons state.pressedToggleButtons
    , Heading.h2
        [ Heading.plaintext "Link with tracking"
        , Heading.css
            [ Css.marginTop Spacing.verticalSpacerPx
            , Css.marginBottom (Css.px 10)
            ]
        ]
    , Button.link "linkExternalWithTracking"
        [ Button.unboundedWidth
        , Button.secondary
        , Button.linkExternalWithTracking
            { url = "#"
            , track = ShowItWorked "ButtonExample" "linkExternalWithTracking clicked"
            }
        ]
    , Heading.h2
        [ Heading.plaintext "Disabled form submit button"
        , Heading.css
            [ Css.marginTop Spacing.verticalSpacerPx
            , Css.marginBottom (Css.px 10)
            ]
        ]
    , form
        []
        [ Button.button "Submit"
            [ Button.submit
            , Button.disabled
            ]
        ]
    , Heading.h2
        [ Heading.plaintext "Disabled button with tooltip"
        , Heading.css
            [ Css.marginTop Spacing.verticalSpacerPx
            , Css.marginBottom (Css.px 10)
            ]
        ]
    , Tooltip.view
        { trigger =
            \attrs ->
                Button.button "Save"
                    [ Button.disabled
                    , Button.custom attrs
                    ]
        , id = "tooltip"
        }
        [ Tooltip.helpfullyDisabled
        , Tooltip.open True
        , Tooltip.paragraph "Reasons why you can't save"
        , Tooltip.onRight
        , Tooltip.fitToContent
        ]
    ]
        |> div []


viewCustomizableExample : Model -> Html Msg
viewCustomizableExample model =
    let
        buttonOrLink =
            case model.buttonType of
                Link ->
                    Button.link

                Button ->
                    Button.button
    in
    buttonOrLink model.label
        (List.map Tuple.second model.attributes)


sizes : List ( String, Button.Attribute msg )
sizes =
    [ ( "small", Button.small )
    , ( "medium", Button.medium )
    , ( "large", Button.large )
    , ( "modal", Button.modal )
    ]


buttonsTable : Html msg
buttonsTable =
    let
        styles =
            [ ( Button.primary, "primary" )
            , ( Button.secondary, "secondary" )
            , ( Button.tertiary, "tertiary" )
            , ( Button.danger, "danger" )
            , ( Button.dangerSecondary, "dangerSecondary" )
            , ( Button.premium, "premium" )
            ]

        exampleRow styleTuple =
            [ tr []
                (td
                    [ css [ verticalAlign middle, Css.borderTop3 (Css.px 1) Css.solid Colors.gray85 ]
                    , Attributes.rowspan 2
                    ]
                    [ code [] [ text (Code.fromModule moduleName (Tuple.second styleTuple)) ] ]
                    :: List.map
                        (exampleCell
                            [ Css.borderTop3 (Css.px 1) Css.solid Colors.gray85
                            , Css.paddingBottom Css.zero |> Css.important
                            ]
                            ( Button.button, "button" )
                            styleTuple
                        )
                        sizes
                    ++ [ td [ css [ verticalAlign middle, Css.borderTop3 (Css.px 1) Css.solid Colors.gray85 ] ]
                            [ code [] [ text "Button.button" ] ]
                       ]
                )
            , tr []
                (List.map (exampleCell [] ( Button.link, "link" ) styleTuple) sizes
                    ++ [ td [ css [ verticalAlign middle ] ]
                            [ code [] [ text "Button.link" ] ]
                       ]
                )
            ]

        exampleCell cellStyle ( view, viewName ) ( style, styleName ) ( sizeName, setSize ) =
            inCell cellStyle <| view (sizeName ++ " " ++ styleName ++ " " ++ viewName) [ setSize, style ]

        inCell style content =
            td
                [ css
                    [ verticalAlign middle
                    , Css.batch style
                    , Css.padding (Css.px 10)
                    ]
                ]
                [ content ]
    in
    List.concat
        [ [ sizes
                |> List.map
                    (\( sizeName, _ ) ->
                        th [ css [ Css.padding2 (Css.px 25) Css.zero ] ]
                            [ code [] [ text (Code.fromModule moduleName sizeName) ]
                            ]
                    )
                |> (\cells -> tr [] (td [] [] :: cells))
          ]
        , List.concatMap exampleRow styles
        ]
        |> table [ css [ Css.borderCollapse Css.collapse, Css.width (Css.pct 100) ] ]
        |> List.singleton
        |> div [ css [ Css.overflow Css.auto ] ]


toggleButtons : Set Int -> Html Msg
toggleButtons pressedToggleButtons =
    div []
        [ Heading.h2
            [ Heading.plaintext "Button toggle"
            , Heading.css
                [ Css.marginTop Spacing.verticalSpacerPx
                , Css.marginBottom (Css.px 10)
                ]
            ]
        , div [ css [ Css.displayFlex, Css.marginBottom (Css.px 20) ] ]
            [ Button.button "5"
                [ Button.toggleButtonPressed (Set.member 0 pressedToggleButtons)
                , Button.onClick (ToggleToggleButton 0)
                ]
            , Button.button "Kindergarten"
                [ Button.toggleButtonPressed (Set.member 1 pressedToggleButtons)
                , Button.onClick (ToggleToggleButton 1)
                ]
            ]
        ]
