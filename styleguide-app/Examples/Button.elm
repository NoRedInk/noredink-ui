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
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Heading.V3 as Heading
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
        |> Control.field "label" (Control.string "Label")
        |> Control.field "attributes"
            (ControlExtra.list
                |> CommonControls.icon moduleName Button.icon
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
            [ ( Button.small, "small" )
            , ( Button.medium, "medium" )
            , ( Button.large, "large" )
            ]

        styles =
            [ ( Button.primary, "primary" )
            , ( Button.secondary, "secondary" )
            , ( Button.tertiary, "tertiary" )
            , ( Button.danger, "danger" )
            , ( Button.premium, "premium" )
            ]

        exampleRow ( style, styleName ) =
            List.concat
                [ [ td
                        [ css
                            [ verticalAlign middle
                            ]
                        ]
                        [ text styleName ]
                  ]
                , List.map (Tuple.first >> exampleCell style) sizes
                ]
                |> tr []

        buttonOrLink =
            case model.buttonType of
                Link ->
                    Button.link

                Button ->
                    Button.button

        exampleCell setStyle setSize =
            buttonOrLink model.label
                (setSize :: setStyle :: List.map Tuple.second model.attributes)
                |> List.singleton
                |> td
                    [ css
                        [ verticalAlign middle
                        , Css.width (Css.px 200)
                        ]
                    ]
    in
    List.concat
        [ [ sizes
                |> List.map (\( _, sizeName ) -> th [] [ text sizeName ])
                |> (\cells -> tr [] (th [] [] :: cells))
          ]
        , List.map exampleRow styles
        ]
        |> table []


toggleButtons : Set Int -> Html Msg
toggleButtons pressedToggleButtons =
    div []
        [ Heading.h3 [ Heading.plaintext "Button toggle" ]
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
