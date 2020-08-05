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
                { onClick = SelectPage
                , selected = state.page
                , width = options.width
                , toUrl = Nothing
                , options = List.take options.count (buildOptions options)
                }
            , Html.h3 [ css [ Css.marginBottom Css.zero ] ]
                [ Html.code [] [ Html.text "viewSelect" ] ]
            , Html.p [ css [ Css.marginTop (Css.px 1) ] ]
                [ Html.text "Use when you only need the ui element. This view is effectively a fancy Radio button." ]
            , SegmentedControl.viewSelect
                { onClick = MaybeSelect
                , options = List.take options.count (buildSelectOptions options.icon)
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
            , label = Debug.toString value
            , value = value
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


buildSelectOptions : Bool -> List (SegmentedControl.SelectOption Int msg)
buildSelectOptions keepIcon =
    let
        buildOption value icon =
            { icon = ifIcon icon
            , label = "Source " ++ Debug.toString (value + 1)
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
    = SelectPage Page
    | MaybeSelect Int
    | ChangeOptions (Control Options)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        SelectPage page ->
            ( { state | page = page }
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
