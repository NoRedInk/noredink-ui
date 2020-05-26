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
import Nri.Ui.SegmentedControl.V9 as SegmentedControl
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.SegmentedControl.V9"
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
                { onClick = SelectNav
                , options = buildOptions "" options [ A, B, C ] [ UiIcon.flag, UiIcon.star, Svg.withColor Colors.greenDark UiIcon.attention ]
                , selected = state.selectedNav
                , width = options.width
                , content = Html.text ("[Content for " ++ Debug.toString state.selectedNav ++ "]")
                }
            , Html.h3 [] [ Html.text "Select only view" ]
            , Html.p [] [ Html.text "Used when you only need the ui element and not a page control." ]
            , SegmentedControl.viewSelect
                { onClick = MaybeSelect
                , options = buildOptions "" options [ One, Two, Three ] [ UiIcon.leaderboard, UiIcon.person, UiIcon.performance ]
                , selected = state.optionallySelected
                , width = options.width
                }
            ]
    , categories = [ Inputs, Widgets ]
    }


buildOptions : String -> Options -> List a -> List Svg -> List (SegmentedControl.Option a)
buildOptions prefix options selections =
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
    List.map2 buildOption selections


type ExampleOptionNav
    = A
    | B
    | C


type ExampleOptionSelect
    = One
    | Two
    | Three


{-| -}
type alias State =
    { selectedNav : ExampleOptionNav
    , optionallySelected : Maybe ExampleOptionSelect
    , optionsControl : Control Options
    }


{-| -}
init : State
init =
    { selectedNav = A
    , optionallySelected = Nothing
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
    = SelectNav ExampleOptionNav
    | MaybeSelect ExampleOptionSelect
    | ChangeOptions (Control Options)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SelectNav id ->
            ( { state | selectedNav = id }
            , Cmd.none
            )

        MaybeSelect id ->
            ( { state | optionallySelected = Just id }
            , Cmd.none
            )

        ChangeOptions newOptions ->
            ( { state | optionsControl = newOptions }
            , Cmd.none
            )
