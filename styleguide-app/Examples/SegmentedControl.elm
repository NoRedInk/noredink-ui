module Examples.SegmentedControl exposing
    ( Msg, State
    , example
    )

{-|

@docs Msg, State
@docs example

-}

import Accessibility.Styled
import Category exposing (Category(..))
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Events
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.SegmentedControl.V8 as SegmentedControl
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.SegmentedControl.V8"
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \state ->
            let
                options =
                    Control.currentValue state.optionsControl
            in
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
                , options = buildOptions "" options [ UiIcon.flag, UiIcon.star, Svg.withColor Colors.greenDark UiIcon.attention ]
                , selected = state.selected
                , width = options.width
                , content = Html.text ("[Content for " ++ Debug.toString state.selected ++ "]")
                }
            , Html.h3 [] [ Html.text "Toggle only view" ]
            , Html.p [] [ Html.text "Used when you only need the ui element and not a page control." ]
            , SegmentedControl.viewToggle
                { onClick = Select
                , options = buildOptions "Toggle-Only " options [ UiIcon.leaderboard, UiIcon.person, UiIcon.performance ]
                , selected = state.selected
                , width = options.width
                }
            , Html.h3 [] [ Html.text "Toggle only view without a default" ]
            , Html.p [] [ Html.text "Used when you only need the ui element and not a page control but don't want a default." ]
            , SegmentedControl.viewOptionalSelectToggle
                { onClick = Select
                , options = buildOptions "Toggle-Only " options [ UiIcon.leaderboard, UiIcon.person, UiIcon.performance ]
                , selected = Just state.selected
                , width = options.width
                }
            ]
    , categories = [ Widgets ]
    }


buildOptions : String -> Options -> List Svg -> List (SegmentedControl.Option ExampleOption)
buildOptions prefix options =
    let
        buildOption option icon =
            { icon =
                if options.icon then
                    Just icon

                else
                    Nothing
            , label = prefix ++ "Choice " ++ Debug.toString option
            , value = option
            }
    in
    List.map2 buildOption [ A, B, C ]


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
