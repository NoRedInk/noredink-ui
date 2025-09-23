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
import Guidance
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Key(..))
import List.Nonempty exposing (Nonempty(..))
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.Extra exposing (withAlpha)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.SegmentedControl.V14 as SegmentedControl
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V6 as Text
import Nri.Ui.Tooltip.V3 as Tooltip
import Nri.Ui.UiIcon.V2 as UiIcon
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
    , init = ( init, Cmd.none )
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = [ viewPreview ]
    , about =
        [ Text.smallBody
            [ Text.html
                [ Html.text "Check out "
                , ClickableText.link "Tessa's demo"
                    [ ClickableText.linkExternal "https://github.com/NoRedInk/NoRedInk/pull/43411"
                    , ClickableText.appearsInline
                    ]
                , Html.text " to get a better sense of whether to use the tabs or radio buttons pattern under the hood."
                ]
            ]
        , Guidance.communicateState moduleName
        ]
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
                , mainType = Just "RootHtml.Html Msg"
                , extraCode =
                    [ "import Nri.Ui.Tooltip.V3 as Tooltip"
                    , Code.newlines
                    , Code.unionType "Msg"
                        [ "FocusAndSelectPage { focus : Maybe String, select : String }"
                        , "OpenTooltip Bool"
                        , "SelectRadio Int"
                        ]
                    ]
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \settings ->
                        [ { sectionName = "view"
                          , code =
                                [ Code.fromModule moduleName "view "
                                , Code.recordMultiline
                                    [ ( "focusAndSelect", "FocusAndSelectPage" )
                                    , ( "options", Code.listOfRecordsMultiline (List.map Tuple.first pageOptions) 2 )
                                    , ( "selected", Code.string (Debug.toString state.page) )
                                    , ( "positioning", Tuple.first options.positioning )
                                    , ( "toUrl", "Nothing" )
                                    ]
                                    1
                                ]
                                    |> String.join ""
                          }
                        , { sectionName = "viewRadioGroup"
                          , code =
                                [ Code.fromModule moduleName "viewRadioGroup"
                                , Code.recordMultiline
                                    [ ( "onSelect", "SelectRadio" )
                                    , ( "options", Code.listOfRecordsMultiline (List.map Tuple.first radioOptions) 2 )
                                    , ( "selected", Debug.toString state.optionallySelected )
                                    , ( "positioning", Tuple.first options.positioning )
                                    , ( "legend", Code.string "SegmentedControls 'viewSelectRadio' example" )
                                    ]
                                    1
                                ]
                                    |> String.join ""
                          }
                        ]
                }
            , Heading.h2
                [ Heading.html [ Html.code [] [ Html.text "view" ], Html.text " Example" ]
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Html.p [ css [ Css.marginTop (Css.px 1) ] ]
                [ Html.text "Use in cases where it would also be reasonable to use Tabs." ]
            , SegmentedControl.view
                { focusAndSelect = FocusAndSelectPage
                , selected = state.page
                , positioning = Tuple.second options.positioning
                , toUrl = Nothing
                , options = List.map Tuple.second pageOptions
                }
            , Heading.h2
                [ Heading.html [ Html.code [] [ Html.text "viewRadioGroup" ], Html.text " Example" ]
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
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
    , categories = [ Layout, Inputs, Tabs ]
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


buildOptions : { options | content : Content, longContent : Bool, tooltips : Bool } -> Maybe Page -> List ( List ( String, String ), SegmentedControl.Option Page Msg )
buildOptions { content, longContent, tooltips } openTooltip =
    let
        buildOption value icon_ =
            let
                ( maybeIcon, label ) =
                    getIconAndLabel content icon_ (Debug.toString value)

                valueStr =
                    Code.string (Debug.toString value)
            in
            ( [ ( "icon", Code.maybe (Maybe.map Tuple.first maybeIcon) )
              , ( "label", "text " ++ valueStr )
              , ( "content", "text " ++ valueStr )
              , ( "value", valueStr )
              , ( "idString", "String.toLower " ++ valueStr )
              , ( "attributes", Code.list [] )
              , ( "tabTooltip"
                , if tooltips then
                    Code.listMultiline
                        [ "Tooltip.plaintext " ++ valueStr
                        , "Tooltip.onToggle OpenTooltip -- generally, you'll want to support more than 1 tooltip type"
                        , "Tooltip.open True -- pass the actual tooltip state"
                        ]
                        4

                  else
                    Code.list []
                )
              ]
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


buildRadioOptions : { options | tooltips : Bool } -> Maybe Int -> Content -> List ( List ( String, String ), SegmentedControl.Radio Int Msg )
buildRadioOptions options currentlyHovered content =
    let
        buildOption value ( text, icon ) =
            let
                ( icon_, label ) =
                    getIconAndLabel content
                        icon
                        ("Source " ++ Debug.toString (value + 1))
            in
            ( [ ( "icon", Code.maybe (Maybe.map Tuple.first icon_) )
              , ( "label", "text " ++ Code.string label )
              , ( "value", String.fromInt value )
              , ( "idString", Code.string (String.fromInt value) )
              , ( "tooltip"
                , if options.tooltips then
                    Code.listMultiline
                        [ "Tooltip.plaintext " ++ Code.string (String.fromInt value)
                        , "Tooltip.onToggle OpenTooltip -- generally, you'll want to support more than 1 tooltip type"
                        , "Tooltip.open True -- pass the actual tooltip state"
                        ]
                        4

                  else
                    Code.list []
                )
              , ( "attributes", Code.list [] )
              ]
            , { icon = Maybe.map Tuple.second icon_
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
        [ ( "Leaderboard", ( "UiIcon.leaderboard", UiIcon.leaderboard ) )
        , ( "Person", ( "UiIcon.person", UiIcon.person ) )
        , ( "Performance", ( "UiIcon.performance", UiIcon.performance ) )
        , ( "Gift", ( "UiIcon.gift", UiIcon.gift ) )
        , ( "Document", ( "UiIcon.document", UiIcon.document ) )
        , ( "Key", ( "UiIcon.key", UiIcon.key ) )
        , ( "Badge", ( "UiIcon.badge", UiIcon.badge ) )
        , ( "Hat", ( "UiIcon.hat", UiIcon.hat ) )
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
                (Nonempty
                    ( "Left (FitContent)"
                    , Control.value
                        ( "SegmentedControl.Left SegmentedControl.FitContent"
                        , SegmentedControl.Left SegmentedControl.FitContent
                        )
                    )
                    [ ( "Left (FillContainer)"
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
            )
        |> Control.field "content" controlContent
        |> Control.field "count"
            (Control.choice
                (List.Nonempty.map (\i -> ( String.fromInt i, Control.value i )) (Nonempty 2 (List.range 3 8)))
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
        (Nonempty ( "Text and icon", Control.value TextAndIcon )
            [ ( "Text", Control.value Text )
            , ( "Icon", Control.value Icon )
            ]
        )


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
