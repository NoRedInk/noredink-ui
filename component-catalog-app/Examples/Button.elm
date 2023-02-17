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
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.UiIcon.V1 as UiIcon
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
                |> ControlExtra.optionalListItem "width"
                    (CommonControls.choice moduleName
                        [ ( "exactWidth 120", Button.exactWidth 120 )
                        , ( "exactWidth 70", Button.exactWidth 70 )
                        , ( "boundedWidth 100 180", Button.boundedWidth { min = 100, max = 180 } )
                        , ( "unboundedWidth", Button.unboundedWidth )
                        , ( "fillContainerWidth", Button.fillContainerWidth )
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
                            ++ " "
                            ++ Code.list (List.map Tuple.first attributes)
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
        [ Heading.plaintext "Interactive examples"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , Heading.h2
        [ Heading.plaintext "Non-interactive examples"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , buttons model
    , toggleButtons state.pressedToggleButtons
    , Button.link "linkExternalWithTracking"
        [ Button.unboundedWidth
        , Button.secondary
        , Button.linkExternalWithTracking
            { url = "#"
            , track = ShowItWorked "ButtonExample" "linkExternalWithTracking clicked"
            }
        ]
    ]
        |> div []


buttons : Model -> Html Msg
buttons model =
    let
        sizes =
            [ ( Button.small, "Button.small" )
            , ( Button.medium, "Button.medium" )
            , ( Button.large, "Button.large" )
            ]

        styles =
            [ ( Button.primary, "Button.primary" )
            , ( Button.secondary, "Button.secondary" )
            , ( Button.tertiary, "Button.tertiary" )
            , ( Button.danger, "Button.danger" )
            , ( Button.premium, "Button.premium" )
            ]

        exampleRow ( style, styleName ) =
            [ tr []
                (td
                    [ css [ verticalAlign middle ]
                    , Attributes.rowspan 2
                    ]
                    [ code [] [ text styleName ] ]
                    :: List.map (Tuple.first >> exampleCell Button.button style) sizes
                    ++ [ td [ css [ verticalAlign middle ] ]
                            [ code [] [ text "Button.button" ] ]
                       ]
                )
            , tr []
                (List.map (Tuple.first >> exampleCell Button.link style) sizes
                    ++ [ td [ css [ verticalAlign middle ] ]
                            [ code [] [ text "Button.link" ] ]
                       ]
                )
            ]

        exampleCell view setStyle setSize =
            inCell <|
                view model.label
                    (setSize :: setStyle :: List.map Tuple.second model.attributes)

        inCell content =
            td
                [ css
                    [ verticalAlign middle
                    , Css.width (Css.px 200)
                    ]
                ]
                [ content ]
    in
    List.concat
        [ [ sizes
                |> List.map (\( _, sizeName ) -> th [] [ code [] [ text sizeName ] ])
                |> (\cells -> tr [] (th [] [] :: cells))
          ]
        , List.concatMap exampleRow styles
        ]
        |> table []


toggleButtons : Set Int -> Html Msg
toggleButtons pressedToggleButtons =
    div []
        [ Heading.h2
            [ Heading.plaintext "Button toggle"
            , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
            ]
        , div [ css [ Css.displayFlex, Css.marginBottom (Css.px 20) ] ]
            [ Button.toggleButton
                { onDeselect = ToggleToggleButton 0
                , onSelect = ToggleToggleButton 0
                , label = "5"
                , pressed = Set.member 0 pressedToggleButtons
                }
            , Button.toggleButton
                { onDeselect = ToggleToggleButton 1
                , onSelect = ToggleToggleButton 1
                , label = "Kindergarten"
                , pressed = Set.member 1 pressedToggleButtons
                }
            ]
        ]
