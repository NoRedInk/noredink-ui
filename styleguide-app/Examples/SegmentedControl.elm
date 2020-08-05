module Examples.SegmentedControl exposing
    ( Msg, State
    , example
    )

{-|

@docs Msg, State
@docs example

-}

import Accessibility.Styled as Html exposing (Html)
import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.SegmentedControl.V10 as SegmentedControl
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.SegmentedControl.V10"
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
            , Html.h3 [ css [ Css.marginBottom Css.zero ] ]
                [ Html.code [] [ Html.text "view" ] ]
            , Html.p [ css [ Css.marginTop (Css.px 1) ] ]
                [ Html.text "Use when you need a page control. This view is effectively a fancy Tab/Tabpanel pairing." ]
            , SegmentedControl.view
                { onClick = SelectNav
                , options = buildOptions "Choice " options (List.range 1 options.count) coloredIcons
                , selected = state.selectedNav
                , width = options.width
                , content = Html.text ("[Content for " ++ Debug.toString state.selectedNav ++ "]")
                , toUrl = Just String.fromInt
                }
            , Html.h3 [ css [ Css.marginBottom Css.zero ] ]
                [ Html.code [] [ Html.text "viewSelect" ] ]
            , Html.p [ css [ Css.marginTop (Css.px 1) ] ]
                [ Html.text "Use when you only need the ui element. This view is effectively a fancy Radio button." ]
            , SegmentedControl.viewSelect
                { onClick = MaybeSelect
                , options = buildOptions "Source " options (List.range 1 options.count) plainIcons
                , selected = state.optionallySelected
                , width = options.width
                }
            ]
    , categories = [ Widgets, Layout ]
    , atomicDesignType = Molecule
    , keyboardSupport =
        [ { keys = [ Enter ], result = "Select the focused control" }
        , { keys = [ Space ], result = "Select the focused control" }
        , { keys = [ Tab ], result = "Focus the next focusable element" }
        , { keys = [ Tab, Shift ], result = "Focus the previous focusable element" }
        ]
    }


coloredIcons : List Svg
coloredIcons =
    [ UiIcon.flag
    , UiIcon.sprout
    , UiIcon.star
    , UiIcon.sapling
    , Svg.withColor Colors.greenDark UiIcon.attention
    , UiIcon.tree
    , UiIcon.premiumLock
    , Svg.withColor Colors.purple UiIcon.activity
    ]


plainIcons : List Svg
plainIcons =
    [ UiIcon.leaderboard
    , UiIcon.person
    , UiIcon.performance
    , UiIcon.gift
    , UiIcon.document
    , UiIcon.key
    , UiIcon.badge
    , UiIcon.hat
    ]


buildOptions : String -> Options -> List a -> List Svg -> List (SegmentedControl.Option a msg)
buildOptions prefix options selections =
    let
        buildOption option icon =
            { icon =
                if options.icon then
                    Just icon

                else
                    Nothing
            , label = prefix ++ Debug.toString option
            , value = option
            , attributes = []
            }
    in
    List.map2 buildOption selections


{-| -}
type alias State =
    { selectedNav : Int
    , optionallySelected : Maybe Int
    , optionsControl : Control Options
    }


{-| -}
init : State
init =
    { selectedNav = 1
    , optionallySelected = Nothing
    , optionsControl = optionsControl
    }


type alias Options =
    { width : SegmentedControl.Width
    , icon : Bool
    , count : Int
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
        |> Control.field "count"
            (Control.choice
                (List.map (\i -> ( String.fromInt i, Control.value i )) (List.range 2 8))
            )


{-| -}
type Msg
    = SelectNav Int
    | MaybeSelect Int
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
