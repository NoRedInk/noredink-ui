module Examples.SegmentedControl exposing
    ( Msg
    , State
    , example
    , init
    , update
    )

{-|

@docs Msg
@docs State
@docs example
@docs init
@docs update

-}

import Accessibility.Styled
import Debug.Control as Control exposing (Control)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Icon.V5 as Icon
import Nri.Ui.SegmentedControl.V7 as SegmentedControl


{-| -}
type Msg
    = Select ExampleOption
    | ChangeOptions (Control Options)


type ExampleOption
    = A
    | B
    | C


{-| -}
type alias State =
    { selected : ExampleOption
    , optionsControl : Control Options
    }


type alias Options =
    { width : SegmentedControl.Width
    , icon : Maybe SegmentedControl.Icon
    , useSpa : Bool
    }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.SegmentedControl.V7"
    , category = Widgets
    , content =
        [ Control.view ChangeOptions state.optionsControl
            |> Html.fromUnstyled
        , let
            options =
                Control.currentValue state.optionsControl

            viewFn =
                if options.useSpa then
                    SegmentedControl.viewSpa Debug.toString

                else
                    SegmentedControl.view
          in
          viewFn
            { onClick = Select
            , options =
                [ A, B, C ]
                    |> List.map
                        (\i ->
                            { icon = options.icon
                            , label = "Option " ++ Debug.toString i
                            , value = i
                            }
                        )
            , selected = state.selected
            , width = options.width
            , content = Html.text ("[Content for " ++ Debug.toString state.selected ++ "]")
            }
        ]
            |> List.map (Html.map parentMessage)
    }


{-| -}
init : { r | help : String } -> State
init assets =
    { selected = A
    , optionsControl =
        Control.record Options
            |> Control.field "width"
                (Control.choice
                    [ ( "FitContent", Control.value SegmentedControl.FitContent )
                    , ( "FillContainer", Control.value SegmentedControl.FillContainer )
                    ]
                )
            |> Control.field "icon"
                (Control.maybe False (Control.value { alt = "Help", icon = Icon.helpSvg assets }))
            |> Control.field "which view function"
                (Control.choice
                    [ ( "view", Control.value False )
                    , ( "viewSpa", Control.value True )
                    ]
                )
    }


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Select id ->
            ( { state | selected = id }
            , Cmd.none
            )

        ChangeOptions newOptions ->
            ( { state | optionsControl = newOptions }
            , Cmd.none
            )
