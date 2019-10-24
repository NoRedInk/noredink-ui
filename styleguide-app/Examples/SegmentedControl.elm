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
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.SegmentedControl.V8 as SegmentedControl
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    let
        options =
            Control.currentValue state.optionsControl
    in
    { name = "Nri.Ui.SegmentedControl.V8"
    , category = Widgets
    , content =
        [ Control.view ChangeOptions state.optionsControl
            |> Html.fromUnstyled
        , let
            viewFn =
                if options.useSpa then
                    SegmentedControl.viewSpa Debug.toString

                else
                    SegmentedControl.view
          in
          viewFn
            { onClick = Select
            , options = buildOptions options
            , selected = state.selected
            , width = options.width
            , content = Html.text ("[Content for " ++ Debug.toString state.selected ++ "]")
            }
        , Html.h3 [] [ Html.text "Toggle only view" ]
        , Html.p [] [ Html.text "Used when you only need the ui element and not a page control." ]
        , SegmentedControl.viewToggle
            { onClick = Select
            , options = buildOptions options
            , selected = state.selected
            , width = options.width
            }
        ]
            |> List.map (Html.map parentMessage)
    }


buildOptions : Options -> List (SegmentedControl.Option ExampleOption)
buildOptions options =
    let
        buildOption ( icon, option ) =
            { icon =
                if options.icon then
                    Just icon

                else
                    Nothing
            , label = "Choice " ++ Debug.toString option
            , value = option
            }
    in
    List.map buildOption
        [ ( UiIcon.flag, A )
        , ( UiIcon.star, B )
        , ( Svg.withColor Colors.greenDark UiIcon.attention, C )
        ]


type ExampleOption
    = A
    | B
    | C


{-| -}
type alias State =
    { selected : ExampleOption
    , optionsControl : Control Options
    }


{-| -}
init : State
init =
    { selected = A
    , optionsControl = optionsControl
    }


type alias Options =
    { width : SegmentedControl.Width
    , icon : Bool
    , useSpa : Bool
    }


optionsControl : Control Options
optionsControl =
    Control.record Options
        |> Control.field "width"
            (Control.choice
                [ ( "FitContent", Control.value SegmentedControl.FitContent )
                , ( "FillContainer", Control.value SegmentedControl.FillContainer )
                ]
            )
        |> Control.field "icon" (Control.bool False)
        |> Control.field "which view function"
            (Control.choice
                [ ( "view", Control.value False )
                , ( "viewSpa", Control.value True )
                ]
            )


{-| -}
type Msg
    = Select ExampleOption
    | ChangeOptions (Control Options)


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
