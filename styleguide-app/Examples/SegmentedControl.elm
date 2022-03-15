module Examples.SegmentedControl exposing
    ( Msg, State
    , example
    )

{-|

@docs Msg, State
@docs example

-}

import Accessibility.Styled as Html exposing (Html)
import Browser.Dom as Dom
import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.SegmentedControl.V14 as SegmentedControl
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Tooltip.V2 as Tooltip
import Nri.Ui.UiIcon.V1 as UiIcon
import String exposing (toLower)
import Task


{-| -}
example : Example State Msg
example =
    { name = "SegmentedControl"
    , version = 14
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = []
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
                { focusAndSelect = FocusAndSelectPage
                , selected = state.page
                , positioning = options.positioning
                , toUrl = Nothing
                , options = List.take options.count (buildOptions options state.pageTooltip)
                }
            , Html.h3 [ css [ Css.marginBottom Css.zero ] ]
                [ Html.code [] [ Html.text "viewRadioGroup" ] ]
            , Html.p [ css [ Css.marginTop (Css.px 1) ] ]
                [ Html.text "Use in cases where it would be reasonable to use radio buttons for the same purpose." ]
            , SegmentedControl.viewRadioGroup
                { legend = "SegmentedControls 'viewSelectRadio' example"
                , onSelect = SelectRadio
                , options = List.take options.count (buildRadioOptions options state.radioTooltip options.content)
                , selected = state.optionallySelected
                , positioning = options.positioning
                }
            ]
    , categories = [ Layout, Inputs ]
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


buildOptions : { options | content : Content, longContent : Bool, tooltips : Bool } -> Maybe Page -> List (SegmentedControl.Option Page Msg)
buildOptions { content, longContent, tooltips } openTooltip =
    let
        buildOption value icon_ =
            let
                ( icon, label ) =
                    getIconAndLabel content icon_ (Html.text (Debug.toString value))
            in
            { icon = icon
            , label = label
            , value = value
            , idString = toLower (Debug.toString value)
            , attributes = []
            , tabTooltip =
                if tooltips then
                    [ Tooltip.plaintext (Debug.toString value)
                    , Tooltip.onHover (PageTooltip value)
                    , Tooltip.open (openTooltip == Just value)
                    ]

                else
                    []
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


buildRadioOptions : { options | tooltips : Bool } -> Maybe Int -> Content -> List (SegmentedControl.Radio Int Msg)
buildRadioOptions options currentlyHovered content =
    let
        buildOption : Int -> ( String, Svg ) -> SegmentedControl.Radio Int Msg
        buildOption value ( text, icon ) =
            let
                ( icon_, label ) =
                    getIconAndLabel content
                        icon
                        (Html.text ("Source " ++ Debug.toString (value + 1)))
            in
            { icon = icon_
            , label = label
            , value = value
            , idString = String.fromInt value
            , tooltip =
                if options.tooltips then
                    [ Tooltip.plaintext text
                    , Tooltip.open (currentlyHovered == Just value)
                    , Tooltip.fitToContent
                    , Tooltip.onHover
                        (\hovered ->
                            HoverRadio
                                (if hovered then
                                    Just value

                                 else
                                    Nothing
                                )
                        )
                    ]

                else
                    []
            , attributes = []
            }
    in
    List.indexedMap buildOption
        [ ( "Leaderboard", UiIcon.leaderboard )
        , ( "Person", UiIcon.person )
        , ( "Performance", UiIcon.performance )
        , ( "Gift", UiIcon.gift )
        , ( "Document", UiIcon.document )
        , ( "Key", UiIcon.key )
        , ( "Badge", UiIcon.badge )
        , ( "Hat", UiIcon.hat )
        ]


{-| -}
type alias State =
    { page : Page
    , pageTooltip : Maybe Page
    , optionallySelected : Maybe Int
    , optionsControl : Control Options
    , radioTooltip : Maybe Int
    }


{-| -}
init : State
init =
    { page = Flag
    , pageTooltip = Nothing
    , optionallySelected = Nothing
    , optionsControl = optionsControl
    , radioTooltip = Nothing
    }


type alias Options =
    { positioning : SegmentedControl.Positioning
    , content : Content
    , count : Int
    , longContent : Bool
    , tooltips : Bool
    }


optionsControl : Control Options
optionsControl =
    Control.record Options
        |> Control.field "positioning"
            (Control.choice
                [ ( "Left (FitContent)", Control.value (SegmentedControl.Left SegmentedControl.FitContent) )
                , ( "Left (FillContainer)", Control.value (SegmentedControl.Left SegmentedControl.FillContainer) )
                , ( "Center", Control.value SegmentedControl.Center )
                ]
            )
        |> Control.field "content" controlContent
        |> Control.field "count"
            (Control.choice
                (List.map (\i -> ( String.fromInt i, Control.value i )) (List.range 2 8))
            )
        |> Control.field "long content" (Control.bool False)
        |> Control.field "tooltips" (Control.bool True)


type Content
    = TextAndIcon
    | Text
    | Icon


controlContent : Control Content
controlContent =
    Control.choice
        [ ( "Text and icon", Control.value TextAndIcon )
        , ( "Text", Control.value Text )
        , ( "Icon", Control.value Icon )
        ]


getIconAndLabel : Content -> svg -> Html msg -> ( Maybe svg, Html msg )
getIconAndLabel content icon_ value =
    case content of
        TextAndIcon ->
            ( Just icon_, value )

        Icon ->
            ( Just icon_, Html.text "" )

        Text ->
            ( Nothing, value )


{-| -}
type Msg
    = FocusAndSelectPage { select : Page, focus : Maybe String }
    | Focused (Result Dom.Error ())
    | PageTooltip Page Bool
    | SelectRadio Int
    | HoverRadio (Maybe Int)
    | ChangeOptions (Control Options)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        FocusAndSelectPage { select, focus } ->
            ( { state | page = select }
            , Maybe.map (Task.attempt Focused << Dom.focus) focus
                |> Maybe.withDefault Cmd.none
            )

        Focused _ ->
            ( state
            , Cmd.none
            )

        PageTooltip page isOpen ->
            ( { state
                | pageTooltip =
                    if isOpen then
                        Just page

                    else
                        Nothing
              }
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

        HoverRadio hovered ->
            ( { state | radioTooltip = hovered }
            , Cmd.none
            )
