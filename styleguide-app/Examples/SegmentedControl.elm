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
import Browser.Dom as Dom
import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.SegmentedControl.V12 as SegmentedControl
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V1 as UiIcon
import String exposing (toLower)
import Task


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.SegmentedControl.V12"
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
                [ Html.text "Use in cases where it would also be reasonable to use Tabs." ]
            , SegmentedControl.view
                { onSelect = SelectPage
                , onFocus = Focus
                , selected = state.page
                , width = options.width
                , toUrl = Nothing
                , options = List.take options.count (buildOptions options)
                }
            , Html.h3 [ css [ Css.marginBottom Css.zero ] ]
                [ Html.code [] [ Html.text "viewRadioGroup" ] ]
            , Html.p [ css [ Css.marginTop (Css.px 1) ] ]
                [ Html.text "Use in cases where it would be reasonable to use radio buttons for the same purpose." ]
            , SegmentedControl.viewRadioGroup
                { legend = "SegmentedControls 'viewSelectRadio' example"
                , onSelect = SelectRadio
                , toString = String.fromInt
                , options = List.take options.count (buildRadioOptions options.icon)
                , selected = state.optionallySelected
                , width = options.width
                }
            ]
    , categories = [ Widgets, Layout ]
    , atomicDesignType = Molecule
    , keyboardSupport =
        [ { keys = [ KeyboardSupport.Tab ]
          , result = "Move focus to the currently-selected Control's content"
          }
        , { keys = [ Arrow KeyboardSupport.Left ]
          , result = "Select the Control to the left of the current selection"
          }
        , { keys = [ Arrow KeyboardSupport.Right ]
          , result = "Select the Control to the right of the current selection"
          }
        ]
    }


type Page
    = Flag
    | Sprout
    | Star
    | Sapling
    | Attention
    | Tree
    | Premium
    | Activity


buildOptions : { options | icon : Bool, longContent : Bool } -> List (SegmentedControl.Option Page Msg)
buildOptions { icon, longContent } =
    let
        buildOption value icon_ =
            { icon =
                if icon then
                    Just icon_

                else
                    Nothing
            , label = Html.text (Debug.toString value)
            , value = value
            , idString = toLower (Debug.toString value)
            , attributes = []
            , content =
                if longContent then
                    Html.div
                        [ css
                            [ Css.maxHeight (Css.px 100)
                            , Css.overflowY Css.auto
                            ]
                        ]
                        [ Html.p [] [ Html.text <| "Content for " ++ Debug.toString value ]
                        , Html.ol [] <|
                            List.map (\i -> Html.li [] [ Html.text (Debug.toString value) ])
                                (List.range 1 20)
                        ]

                else
                    Html.text <| "Content for " ++ Debug.toString value
            }
    in
    [ buildOption Flag UiIcon.flag
    , buildOption Sprout UiIcon.sprout
    , buildOption Star UiIcon.star
    , buildOption Sapling UiIcon.sapling
    , buildOption Attention <| Svg.withColor Colors.greenDark UiIcon.attention
    , buildOption Tree UiIcon.tree
    , buildOption Premium UiIcon.premiumLock
    , buildOption Activity <| Svg.withColor Colors.purple UiIcon.activity
    ]


buildRadioOptions : Bool -> List (SegmentedControl.Radio Int msg)
buildRadioOptions keepIcon =
    let
        buildOption value icon =
            { icon = ifIcon icon
            , label = Html.text ("Source " ++ Debug.toString (value + 1))
            , value = value
            , attributes = []
            }

        ifIcon icon =
            if keepIcon then
                Just icon

            else
                Nothing
    in
    List.indexedMap buildOption
        [ UiIcon.leaderboard
        , UiIcon.person
        , UiIcon.performance
        , UiIcon.gift
        , UiIcon.document
        , UiIcon.key
        , UiIcon.badge
        , UiIcon.hat
        ]


{-| -}
type alias State =
    { page : Page
    , optionallySelected : Maybe Int
    , optionsControl : Control Options
    }


{-| -}
init : State
init =
    { page = Flag
    , optionallySelected = Nothing
    , optionsControl = optionsControl
    }


type alias Options =
    { width : SegmentedControl.Width
    , icon : Bool
    , count : Int
    , longContent : Bool
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
        |> Control.field "icon" (Control.bool True)
        |> Control.field "count"
            (Control.choice
                (List.map (\i -> ( String.fromInt i, Control.value i )) (List.range 2 8))
            )
        |> Control.field "long content" (Control.bool False)


{-| -}
type Msg
    = Focus String
    | Focused (Result Dom.Error ())
    | SelectPage Page
    | SelectRadio Int
    | ChangeOptions (Control Options)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        Focus id ->
            ( state
            , Task.attempt Focused (Dom.focus id)
            )

        Focused _ ->
            ( state
            , Cmd.none
            )

        SelectPage page ->
            ( { state | page = page }
            , Cmd.none
            )

        SelectRadio id ->
            ( { state | optionallySelected = Just id }
            , Cmd.none
            )

        ChangeOptions newOptions ->
            ( { state | optionsControl = newOptions }
            , Cmd.none
            )
