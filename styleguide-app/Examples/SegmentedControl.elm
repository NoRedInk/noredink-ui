module Examples.SegmentedControl exposing
    ( Msg, State
    , example
    )

{-|

@docs Msg, State
@docs example

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import Browser.Dom as Dom
import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Colors.Extra exposing (withAlpha)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.SegmentedControl.V14 as SegmentedControl
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Tooltip.V3 as Tooltip
import Nri.Ui.UiIcon.V1 as UiIcon
import String exposing (toLower)
import Task


moduleName : String
moduleName =
    "SegmentedControl"


version : Int
version =
    14


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = [ viewPreview ]
    , view =
        \ellieLinkConfig state ->
            let
                options =
                    Control.currentValue state.optionsControl

                pageOptions =
                    List.take options.count (buildOptions options state.pageTooltip)

                radioOptions =
                    List.take options.count (buildRadioOptions options state.radioTooltip options.content)
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = ChangeOptions
                , settings = state.optionsControl
                , mainType = Just "RootHtml.Html msg"
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \settings ->
                        [ { sectionName = "view"
                          , code =
                                [ moduleName ++ ".view "
                                , "    { focusAndSelect = FocusAndSelectPage"
                                , "    , options = " ++ Code.list (List.map Tuple.first pageOptions)
                                , "    , selected = \"" ++ Debug.toString state.page ++ "\""
                                , "    , positioning = " ++ Tuple.first options.positioning
                                , "    , toUrl = Nothing"
                                , "    }"
                                ]
                                    |> String.join "\n"
                          }
                        , { sectionName = "viewRadioGroup"
                          , code =
                                [ moduleName ++ ".viewRadioGroup"
                                , "    { onSelect = SelectRadio"
                                , "    , options = " ++ Code.list (List.map Tuple.first radioOptions)
                                , "    , selected = " ++ Debug.toString state.optionallySelected
                                , "    , positioning = " ++ Tuple.first options.positioning
                                , "    , legend = \"SegmentedControls 'viewSelectRadio' example\""
                                , "    }"
                                ]
                                    |> String.join "\n"
                          }
                        ]
                }
            , Html.h3 [ css [ Css.marginBottom Css.zero ] ]
                [ Html.code [] [ Html.text "view" ] ]
            , Html.p [ css [ Css.marginTop (Css.px 1) ] ]
                [ Html.text "Use in cases where it would also be reasonable to use Tabs." ]
            , SegmentedControl.view
                { focusAndSelect = FocusAndSelectPage
                , selected = state.page
                , positioning = Tuple.second options.positioning
                , toUrl = Nothing
                , options = List.map Tuple.second pageOptions
                }
            , Html.h3 [ css [ Css.marginBottom Css.zero ] ]
                [ Html.code [] [ Html.text "viewRadioGroup" ] ]
            , Html.p [ css [ Css.marginTop (Css.px 1) ] ]
                [ Html.text "Use in cases where it would be reasonable to use radio buttons for the same purpose." ]
            , SegmentedControl.viewRadioGroup
                { legend = "SegmentedControls 'viewSelectRadio' example"
                , onSelect = SelectRadio
                , options = List.map Tuple.second radioOptions
                , selected = state.optionallySelected
                , positioning = Tuple.second options.positioning
                }
            ]
    , categories = [ Layout, Inputs ]
    , extraResources = []
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


viewPreview : Html msg
viewPreview =
    Html.div
        [ Role.img
        , Aria.hidden True
        , css
            [ Css.displayFlex
            , Css.justifyContent Css.stretch
            , Css.color Colors.navy
            , Fonts.baseFont
            , Css.fontSize (Css.px 15)
            , Css.fontWeight Css.bold
            ]
        ]
        [ Html.div
            [ css
                [ Css.padding2 (Css.px 6) (Css.px 15)
                , Css.border3 (Css.px 1) Css.solid Colors.azure
                , Css.borderTopLeftRadius (Css.px 8)
                , Css.borderBottomLeftRadius (Css.px 8)
                , Css.backgroundColor Colors.glacier
                , Css.boxShadow5 Css.inset Css.zero (Css.px 3) Css.zero (withAlpha 0.2 Colors.gray20)
                ]
            ]
            [ Html.text "Abc" ]
        , Html.div
            [ css
                [ Css.padding2 (Css.px 6) (Css.px 15)
                , Css.border3 (Css.px 1) Css.solid Colors.azure
                , Css.borderTopRightRadius (Css.px 8)
                , Css.borderBottomRightRadius (Css.px 8)
                , Css.backgroundColor Colors.white
                , Css.boxShadow5 Css.inset Css.zero (Css.px -2) Css.zero Colors.azure
                ]
            ]
            [ Html.text "Def" ]
        ]


type Page
    = Flag
    | Sprout
    | Star
    | Sapling
    | Attention
    | Tree
    | Premium
    | Activity


buildOptions : { options | content : Content, longContent : Bool, tooltips : Bool } -> Maybe Page -> List ( String, SegmentedControl.Option Page Msg )
buildOptions { content, longContent, tooltips } openTooltip =
    let
        buildOption value icon_ =
            let
                ( maybeIcon, label ) =
                    getIconAndLabel content icon_ (Debug.toString value)

                valueStr =
                    "\"" ++ Debug.toString value ++ "\""
            in
            ( [ "{ icon = " ++ Debug.toString (Maybe.map Tuple.first maybeIcon)
              , ", label = text " ++ valueStr
              , ", value = " ++ valueStr
              , ", idString = String.toLower " ++ valueStr
              , ", attributes = []"
              , ", tabTooltip = "
                    ++ (if tooltips then
                            ("\n\t\t[ Tooltip.plaintext " ++ valueStr)
                                ++ ("\n\t\t, Tooltip.onToggle (OpenTooltip " ++ valueStr ++ ")")
                                ++ ("\n\t\t, Tooltip.open (openTooltip == Just " ++ valueStr ++ ")")
                                ++ "\n\t\t]"

                        else
                            "[]"
                       )
              , ", content = text \"...\""
              , "}"
              ]
                |> String.join "\n\t  "
            , { icon = Maybe.map Tuple.second maybeIcon
              , label = Html.text label
              , value = value
              , idString = toLower (Debug.toString value)
              , attributes = []
              , tabTooltip =
                    if tooltips then
                        [ Tooltip.plaintext (Debug.toString value)
                        , Tooltip.onToggle (PageTooltip value)
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
            )
    in
    [ buildOption Flag ( "UiIcon.flag", UiIcon.flag )
    , buildOption Sprout ( "UiIcon.sprout", UiIcon.sprout )
    , buildOption Star ( "UiIcon.star", UiIcon.star )
    , buildOption Sapling ( "UiIcon.sapling", UiIcon.sapling )
    , buildOption Attention ( "UiIcon.attention", Svg.withColor Colors.greenDark UiIcon.attention )
    , buildOption Tree ( "UiIcon.tree", UiIcon.tree )
    , buildOption Premium ( "UiIcon.premiumLock", UiIcon.premiumLock )
    , buildOption Activity ( "UiIcon.activity", Svg.withColor Colors.purple UiIcon.activity )
    ]


buildRadioOptions : { options | tooltips : Bool } -> Maybe Int -> Content -> List ( String, SegmentedControl.Radio Int Msg )
buildRadioOptions options currentlyHovered content =
    let
        buildOption : Int -> ( String, Svg ) -> ( String, SegmentedControl.Radio Int Msg )
        buildOption value ( text, icon ) =
            let
                ( icon_, label ) =
                    getIconAndLabel content
                        icon
                        ("Source " ++ Debug.toString (value + 1))
            in
            ( [ "{ icon = " ++ Debug.toString (Maybe.map (\_ -> "UiIcon." ++ toLower label) icon_)
              , ", label = Html.text " ++ label
              , ", value = " ++ String.fromInt value
              , ", idString = String.fromInt " ++ String.fromInt value
              , ", tooltip = "
                    ++ (if options.tooltips then
                            ("\n\t\t[ Tooltip.plaintext " ++ String.fromInt value)
                                ++ ("\n\t\t, Tooltip.onToggle (OpenTooltip " ++ String.fromInt value ++ ")")
                                ++ ("\n\t\t, Tooltip.open (openTooltip == Just " ++ String.fromInt value ++ ")")
                                ++ "\n\t\t]"

                        else
                            "[]"
                       )
              , ", attributes = []"
              , "}"
              ]
                |> String.join "\n\t  "
            , { icon = icon_
              , label = Html.text label
              , value = value
              , idString = String.fromInt value
              , tooltip =
                    if options.tooltips then
                        [ Tooltip.plaintext text
                        , Tooltip.open (currentlyHovered == Just value)
                        , Tooltip.fitToContent
                        , Tooltip.onToggle
                            (\hovered ->
                                HoverRadio
                                    (if hovered then
                                        Just value

                                     else
                                        Nothing
                                    )
                            )
                        , Tooltip.auxiliaryDescription
                        ]

                    else
                        []
              , attributes = []
              }
            )
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
    { positioning : ( String, SegmentedControl.Positioning )
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
                [ ( "Left (FitContent)"
                  , Control.value
                        ( "SegmentedControl.Left SegmentedControl.FitContent"
                        , SegmentedControl.Left SegmentedControl.FitContent
                        )
                  )
                , ( "Left (FillContainer)"
                  , Control.value
                        ( "SegmentedControl.Left SegmentedControl.FillContainer"
                        , SegmentedControl.Left SegmentedControl.FillContainer
                        )
                  )
                , ( "Center"
                  , Control.value
                        ( "SegmentedControl.Center"
                        , SegmentedControl.Center
                        )
                  )
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


getIconAndLabel : Content -> icon -> String -> ( Maybe icon, String )
getIconAndLabel content icon_ value =
    case content of
        TextAndIcon ->
            ( Just icon_, value )

        Icon ->
            ( Just icon_, "" )

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
