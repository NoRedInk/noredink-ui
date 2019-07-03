module Examples.Button exposing (Msg, State, example, init, update)

{- \
   @docs Msg, State, example, init, update,
-}

import Css exposing (middle, verticalAlign)
import Debug.Control as Control exposing (Control)
import Headings
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, id)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample, ModuleMessages)
import Nri.Ui.AssetPath exposing (Asset)
import Nri.Ui.Button.V9 as Button exposing (ButtonOrLink)
import Nri.Ui.Icon.V5 as Icon
import Nri.Ui.Svg.V1 as NriSvg exposing (Svg)
import Nri.Ui.Text.V3 as Text


{-| -}
type State
    = State (Control Model)


{-| -}
type ButtonType
    = Button
    | Link


{-| -}
example :
    (String -> ModuleMessages Msg parentMsg)
    -> State
    -> ModuleExample parentMsg
example unnamedMessages state =
    let
        messages =
            unnamedMessages "ButtonExample"
    in
    { name = "Nri.Ui.Button.V9"
    , category = Buttons
    , content = [ viewButtonExamples messages state ]
    }


{-| -}
type Msg
    = SetState State
    | NoOp


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SetState newState ->
            ( newState, Cmd.none )

        NoOp ->
            ( state, Cmd.none )



-- INTERNAL


type alias Model =
    { label : String
    , icon : Maybe Svg
    , width : ButtonOrLink () -> ButtonOrLink ()
    , buttonType : ButtonType
    , state : Button.ButtonState
    }


{-| -}
init : { r | performance : String, lock : String } -> State
init assets =
    Control.record Model
        |> Control.field "label" (Control.string "Label")
        |> Control.field "icon" (iconChoice assets)
        |> Control.field "width"
            (Control.choice
                ( "exactWidth 120", Control.value (Button.exactWidth 120) )
                [ ( "exactWidth 70", Control.value (Button.exactWidth 70) )
                , ( "unboundedWidth", Control.value Button.unboundedWidth )
                , ( "fillContainerWidth", Control.value Button.fillContainerWidth )
                ]
            )
        |> Control.field "button type"
            (Control.choice
                ( "button", Control.value Button )
                [ ( "link", Control.value Link )
                ]
            )
        |> Control.field "state (button only)"
            (Control.choice
                ( Debug.toString Button.Enabled, Control.value Button.Enabled )
                (List.map (\x -> ( Debug.toString x, Control.value x ))
                    [ Button.Disabled
                    , Button.Error
                    , Button.Unfulfilled
                    , Button.Loading
                    , Button.Success
                    ]
                )
            )
        |> State


iconChoice : { r | performance : String, lock : String } -> Control.Control (Maybe Svg)
iconChoice assets =
    Control.choice
        ( "Nothing", Control.value Nothing )
        [ ( "Just Performance"
          , Icon.performance assets
                |> Icon.decorativeIcon
                |> NriSvg.fromHtml
                |> Just
                |> Control.value
          )
        , ( "Just Lock"
          , Icon.lock assets
                |> Icon.decorativeIcon
                |> NriSvg.fromHtml
                |> Just
                |> Control.value
          )
        ]


viewButtonExamples :
    ModuleMessages Msg parentMsg
    -> State
    -> Html parentMsg
viewButtonExamples messages (State control) =
    let
        model =
            Control.currentValue control
    in
    [ Control.view (State >> SetState >> messages.wrapper) control
        |> fromUnstyled
    , buttons messages model
    , toggleButtons messages
    , Button.delete
        { label = "Delete Something"
        , onClick = messages.showItWorked "delete"
        }
    , Button.linkExternalWithTracking
        (messages.showItWorked "linkExternalWithTracking clicked")
        { size = Button.Medium
        , style = Button.Secondary
        , width = Button.WidthUnbounded
        , label = "linkExternalWithTracking"
        , icon = Nothing
        , url = "#"
        }
    ]
        |> div []


buttons :
    ModuleMessages Msg parentMsg
    -> Model
    -> Html parentMsg
buttons messages model =
    let
        sizes =
            [ ( Button.small, "Button.small" )
            , ( Button.medium, "Button.medium" )
            , ( Button.large, "Button.large" )
            ]

        styles =
            [ ( Button.primary, "Button.primary" )
            , ( Button.secondary, "Button.secondary" )
            , ( Button.danger, "Button.danger" )
            , ( Button.premium, "Button.premium" )
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

        addAttributes button =
            [ Maybe.map Button.withIcon model.icon
            ]
                |> List.filterMap identity
                |> List.foldl (\addAttr b -> addAttr b) button

        exampleCell setStyle setSize =
            (Button.build
                |> addAttributes
                |> setSize
                |> setStyle
                |> Button.withLabel model.label
                |> model.width
                |> Button.setButtonState model.state
                |> Button.href ""
                |> Button.onClick (messages.showItWorked "Button clicked!")
                |> (case model.buttonType of
                        Link ->
                            Button.renderLink

                        Button ->
                            Button.renderButton
                   )
            )
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


toggleButtons : ModuleMessages Msg parentMsg -> Html parentMsg
toggleButtons messages =
    div []
        [ Headings.h3 [ text "Button toggle" ]
        , Button.toggleButton
            { onDeselect = messages.showItWorked "onDeselect"
            , onSelect = messages.showItWorked "onSelect"
            , label = "5"
            , pressed = False
            }
        , Button.toggleButton
            { onDeselect = messages.showItWorked "onDeselect"
            , onSelect = messages.showItWorked "onSelect"
            , label = "5"
            , pressed = True
            }
        ]
