module Examples.Button exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Aria as Aria
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
import Examples.RadioButtonDotless as RadioButtonDotlessExample
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Message.V4 as Message
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Tooltip.V3 as Tooltip
import Nri.Ui.UiIcon.V1 as UiIcon
import Routes
import Set exposing (Set)


version : Int
version =
    10


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
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
        [ let
            url =
                Routes.toString <| Routes.Doodad RadioButtonDotlessExample.example
          in
          Message.view
            [ Message.markdown <| "Looking for a group of buttons where only one button is selectable at a time? Check out [RadioButtonDotless](" ++ url ++ ")"
            ]
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
        |> Control.field "attributes"
            (ControlExtra.list
                |> CommonControls.icon moduleName Button.icon
                |> CommonControls.rightIcon moduleName Button.rightIcon
                |> ControlExtra.optionalListItem "size"
                    (CommonControls.choice moduleName
                        [ ( "small", Button.small )
                        , ( "medium", Button.medium )
                        , ( "large", Button.large )
                        , ( "modal", Button.modal )
                        ]
                    )
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
                |> ControlExtra.optionalBoolListItem "disabled" ( "disabled", Button.disabled )
                |> ControlExtra.optionalListItem "state (button only)"
                    (CommonControls.choice moduleName
                        [ ( "error", Button.error )
                        , ( "unfulfilled", Button.unfulfilled )
                        , ( "loading", Button.loading )
                        , ( "success", Button.success )
                        ]
                    )
                |> ControlExtra.optionalListItem "toggleButtonPressed"
                    (Control.map
                        (\bool ->
                            ( "toggleButtonPressed " ++ Code.bool bool
                            , Button.toggleButtonPressed bool
                            )
                        )
                        (Control.bool True)
                    )
                |> ControlExtra.optionalBoolListItem "submit (button only)"
                    ( "Button.submit", Button.submit )
                |> ControlExtra.optionalBoolListItem "opensModal (button only)"
                    ( "Button.opensModal", Button.opensModal )
                |> ControlExtra.optionalBoolListItem "hideIconForMobile"
                    ( "Button.hideIconForMobile", Button.hideIconForMobile )
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


tooltipId : String
tooltipId =
    "tooltip"


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
                        moduleName
                            ++ "."
                            ++ fName
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
        [ Heading.plaintext "Interactive example"
        , Heading.css [ Css.margin2 Spacing.verticalSpacerPx Css.zero ]
        ]
    , viewCustomizableExample model
    , Heading.h2
        [ Heading.plaintext "Non-interactive examples"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , buttonsTable
    , toggleButtons state.pressedToggleButtons
    , Button.link "linkExternalWithTracking"
        [ Button.unboundedWidth
        , Button.secondary
        , Button.linkExternalWithTracking
            { url = "#"
            , track = ShowItWorked "ButtonExample" "linkExternalWithTracking clicked"
            }
        ]
    , form
        [ css [ Css.marginTop Spacing.verticalSpacerPx ] ]
        [ Button.button "Submit"
            [ Button.submit
            , Button.disabled
            ]
        ]
    , div [ css [ Css.marginTop Spacing.verticalSpacerPx ] ]
        [ Tooltip.view
            { trigger =
                \attrs ->
                    Button.button "Disabled with tooltip"
                        [ Button.disabled
                        , Button.custom (Aria.describedBy [ tooltipId ] :: attrs)
                        ]
            , id = tooltipId
            }
            [ Tooltip.open True
            , Tooltip.paragraph "The quick brown fox jumps over the lazy dog."
            , Tooltip.onRight
            ]
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


buttonsTable : Html msg
buttonsTable =
    let
        sizes =
            [ ( Button.small, "small" )
            , ( Button.medium, "medium" )
            , ( Button.large, "large" )
            ]

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

        exampleCell cellStyle ( view, viewName ) ( style, styleName ) ( setSize, sizeName ) =
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
                    (\( _, sizeName ) ->
                        th [ css [ Css.padding2 (Css.px 25) Css.zero ] ]
                            [ code [] [ text (Code.fromModule moduleName sizeName) ]
                            ]
                    )
                |> (\cells -> tr [] (td [] [] :: cells))
          ]
        , List.concatMap exampleRow styles
        ]
        |> table [ css [ Css.borderCollapse Css.collapse ] ]


toggleButtons : Set Int -> Html Msg
toggleButtons pressedToggleButtons =
    div []
        [ Heading.h2
            [ Heading.plaintext "Button toggle"
            , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
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
