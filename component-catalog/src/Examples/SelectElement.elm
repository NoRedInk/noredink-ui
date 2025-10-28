module Examples.SelectElement exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Guidance
import Html.Styled exposing (Html, div, p, span, text)
import Html.Styled.Attributes as Attributes exposing (css)
import KeyboardSupport
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.SelectElement.V2 as SelectElement
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V6 as Text
import Nri.Ui.Tooltip.V3 as Tooltip
import Nri.Ui.UiIcon.V2 as UiIcon


moduleName : String
moduleName =
    "SelectElement"


version : Int
version =
    2


type SampleItem
    = Mercury
    | Venus
    | Earth
    | Mars
    | Jupiter
    | Saturn
    | Uranus
    | Neptune
    | Pluto


sampleItemToString : SampleItem -> String
sampleItemToString sampleItem =
    case sampleItem of
        Mercury ->
            "Mercury"

        Venus ->
            "Venus"

        Earth ->
            "Earth"

        Mars ->
            "Mars"

        Jupiter ->
            "Jupiter"

        Saturn ->
            "Saturn"

        Uranus ->
            "Uranus"

        Neptune ->
            "Neptune"

        Pluto ->
            "Pluto"


stringToSampleItem : String -> Maybe SampleItem
stringToSampleItem str =
    case str of
        "Mercury" ->
            Just Mercury

        "Venus" ->
            Just Venus

        "Earth" ->
            Just Earth

        "Mars" ->
            Just Mars

        "Jupiter" ->
            Just Jupiter

        "Saturn" ->
            Just Saturn

        "Uranus" ->
            Just Uranus

        "Neptune" ->
            Just Neptune

        "Pluto" ->
            Just Pluto

        _ ->
            Nothing


sampleItemToLabel : SampleItem -> String
sampleItemToLabel =
    sampleItemToString


type TooltipId
    = EarthGradingTooltip


exampleOptionItems : State -> List (SelectElement.OptionItem SampleItem Msg)
exampleOptionItems state =
    [ SelectElement.Group "Inner Planets"
        [ { content = Html.Styled.text (sampleItemToLabel Mercury)
          , triggerLabel = sampleItemToLabel Mercury
          , value = Mercury
          }
        , { content = Html.Styled.text (sampleItemToLabel Venus)
          , triggerLabel = sampleItemToLabel Venus
          , value = Venus
          }
        , { content =
                div [ css [ Css.displayFlex, Css.alignItems Css.center ] ]
                    [ span [ css [ Css.marginRight (Css.px 8) ] ]
                        [ Tooltip.view
                            { id = "earth-tooltip"
                            , trigger =
                                \attrs ->
                                    span
                                        (css [ Css.cursor Css.pointer ] :: attrs)
                                        [ UiIcon.gradingAssistant
                                            |> Svg.withColor Colors.azure
                                            |> Svg.withWidth (Css.px 16)
                                            |> Svg.withHeight (Css.px 16)
                                            |> Svg.toHtml
                                        ]
                            }
                            [ Tooltip.plaintext "Our home planet."
                            , Tooltip.onToggle (ToggleTooltip EarthGradingTooltip)
                            , Tooltip.open (state.openTooltip == Just EarthGradingTooltip)
                            , Tooltip.smallPadding
                            , Tooltip.onRight
                            , Tooltip.disclosure { triggerId = "earth-tooltip", lastId = Nothing }
                            ]
                        ]
                    , text (sampleItemToLabel Earth)
                    ]
          , triggerLabel = sampleItemToLabel Earth
          , value = Earth
          }
        , { content = Html.Styled.text (sampleItemToLabel Mars)
          , triggerLabel = sampleItemToLabel Mars
          , value = Mars
          }
        ]
    , SelectElement.Group "Gas Giants"
        [ { content = Html.Styled.text (sampleItemToLabel Jupiter)
          , triggerLabel = sampleItemToLabel Jupiter
          , value = Jupiter
          }
        , { content = Html.Styled.text (sampleItemToLabel Saturn)
          , triggerLabel = sampleItemToLabel Saturn
          , value = Saturn
          }
        , { content = Html.Styled.text (sampleItemToLabel Uranus)
          , triggerLabel = sampleItemToLabel Uranus
          , value = Uranus
          }
        , { content = Html.Styled.text (sampleItemToLabel Neptune)
          , triggerLabel = sampleItemToLabel Neptune
          , value = Neptune
          }
        ]
    , SelectElement.Item
        { content = Html.Styled.text (sampleItemToLabel Pluto)
        , triggerLabel = sampleItemToLabel Pluto
        , value = Pluto
        }
    ]


previewOptionItems : List (SelectElement.OptionItem SampleItem Never)
previewOptionItems =
    [ SelectElement.Group "Inner Planets"
        [ { content = Html.Styled.div [] []
          , triggerLabel = "Mercury"
          , value = Mercury
          }
        , { content = Html.Styled.div [] []
          , triggerLabel = "Venus"
          , value = Venus
          }
        , { content = Html.Styled.div [] []
          , triggerLabel = "Earth"
          , value = Earth
          }
        , { content = Html.Styled.div [] []
          , triggerLabel = "Mars"
          , value = Mars
          }
        ]
    , SelectElement.Group "Gas Giants"
        [ { content = Html.Styled.div [] []
          , triggerLabel = "Jupiter"
          , value = Jupiter
          }
        , { content = Html.Styled.div [] []
          , triggerLabel = "Saturn"
          , value = Saturn
          }
        , { content = Html.Styled.div [] []
          , triggerLabel = "Uranus"
          , value = Uranus
          }
        , { content = Html.Styled.div [] []
          , triggerLabel = "Neptune"
          , value = Neptune
          }
        ]
    , SelectElement.Item
        { content = Html.Styled.div [] []
        , triggerLabel = "Pluto"
        , value = Pluto
        }
    ]


disabledOptionsList : List SampleItem
disabledOptionsList =
    [ Jupiter, Saturn ]


firstSelectableSampleItem : List (SelectElement.OptionItem SampleItem Msg) -> Maybe SampleItem
firstSelectableSampleItem items =
    case items of
        [] ->
            Nothing

        (SelectElement.Item opt) :: _ ->
            Just opt.value

        (SelectElement.Group _ subOptions) :: rest ->
            case subOptions of
                [] ->
                    firstSelectableSampleItem rest

                opt :: _ ->
                    Just opt.value


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = ( init, Cmd.none )
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Inputs ]
    , keyboardSupport =
        [ -- Trigger Button (when dropdown is closed)
          { keys = [ KeyboardSupport.Enter ]
          , result = "Opens the dropdown. Focus moves to the first enabled option (or the currently selected option if enabled)."
          }
        , { keys = [ KeyboardSupport.Space ]
          , result = "Opens the dropdown. Focus moves to the first enabled option (or the currently selected option if enabled)."
          }
        , { keys = [ KeyboardSupport.Arrow KeyboardSupport.Down ]
          , result = "Opens the dropdown. Focus moves to the first enabled option (or selected if enabled)."
          }
        , { keys = [ KeyboardSupport.Arrow KeyboardSupport.Up ]
          , result = "Opens the dropdown. Focus moves to the last enabled option (or selected if enabled)."
          }

        -- Dropdown / Listbox (when open)
        , { keys = [ KeyboardSupport.Arrow KeyboardSupport.Down ]
          , result = "Moves focus to the next enabled option, wrapping from last to first."
          }
        , { keys = [ KeyboardSupport.Arrow KeyboardSupport.Up ]
          , result = "Moves focus to the previous enabled option, wrapping from first to last."
          }
        , { keys = []
          , result = "Home: Moves focus to the first enabled option."
          }
        , { keys = []
          , result = "End: Moves focus to the last enabled option."
          }
        , { keys = [ KeyboardSupport.Enter ]
          , result = "Selects the focused option, closes the dropdown, and returns focus to the trigger button."
          }
        , { keys = [ KeyboardSupport.Space ]
          , result = "Selects the focused option (if not typing in search), closes the dropdown, and returns focus to the trigger button."
          }
        , { keys = []
          , result = "Printable Characters (e.g. 'a', 'b', '1'): Typeahead - Moves focus to the next enabled option starting with the typed characters. Search clears after a short delay."
          }
        , { keys = [ KeyboardSupport.Esc ]
          , result = "Closes the dropdown. Focus returns to the trigger button."
          }
        , { keys = [ KeyboardSupport.Tab ]
          , result = "Closes the dropdown and moves focus to the next focusable element in the document order."
          }
        ]
    , preview =
        [ SelectElement.view "Select..."
            [ SelectElement.optionItems previewOptionItems
            , SelectElement.valueToString sampleItemToString
            , SelectElement.stringToValue stringToSampleItem
            , SelectElement.triggerId "preview-trigger"
            , SelectElement.popoverId "preview-popover"
            , SelectElement.label "Preview Select"
            , SelectElement.customHtmlAttributesForTrigger [ Attributes.tabindex -1 ]
            , SelectElement.disabled True
            , SelectElement.containerCss [ Css.marginBottom (Css.px 100) ]
            ]
        ]
    , about =
        [ Text.smallBody [ Text.markdown "This component uses a `<select-element>` custom HTML element to provide an accessible select/listbox." ]
        , Text.smallBody [ Text.markdown "The custom element handles ARIA attributes, keyboard navigation, and popover behavior." ]
        , Guidance.message moduleName
        ]
    , view = viewExamplePage
    }


viewExamplePage : EllieLink.Config -> State -> List (Html Msg)
viewExamplePage ellieLinkConfig state =
    let
        settings =
            Control.currentValue state.control

        attrs =
            [ SelectElement.optionItems (exampleOptionItems state)
            , SelectElement.valueToString sampleItemToString
            , SelectElement.stringToValue stringToSampleItem
            , SelectElement.selectedValue state.currentSelection
            , SelectElement.onSelect OptionSelectedMsg
            , SelectElement.triggerId settings.triggerId
            , SelectElement.popoverId settings.popoverId
            , SelectElement.label "My Dynamic Select"
            , SelectElement.hideLabel settings.hideLabel
            , SelectElement.disabled settings.disabled
            , SelectElement.loading settings.loading
            , SelectElement.disableWhen (\item -> List.member item disabledOptionsList || settings.disableAllOptions)
            , if settings.icon then
                SelectElement.icon UiIcon.checkmark

              else
                SelectElement.Attribute identity
            , if String.isEmpty settings.error then
                SelectElement.Attribute identity

              else
                SelectElement.error settings.error
            , if String.isEmpty settings.guidance then
                SelectElement.Attribute identity

              else
                SelectElement.guidance settings.guidance
            , SelectElement.containerCss
                ([ Css.marginBottom (Css.px 100) ]
                    ++ (if settings.containerCss then
                            [ Css.backgroundColor (Css.hex "FFFACD") ]

                        else
                            []
                       )
                )
            , SelectElement.noMargin settings.noMargin
            ]
                ++ (if settings.title then
                        [ SelectElement.title "My Customizable Select (Title)" ]

                    else
                        []
                   )
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = UpdateControl
        , settings = state.control
        , mainType = Just "Html Msg"
        , extraCode =
            [ Code.unionType "SampleItem" (List.map sampleItemToString [ Mercury, Venus, Earth, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto ])
            , """
sampleItemToString : SampleItem -> String
sampleItemToString sampleItem =
    case sampleItem of
        Mercury -> "Mercury"
        Venus   -> "Venus"
        Earth   -> "Earth"
        Mars    -> "Mars"
        Jupiter -> "Jupiter"
        Saturn  -> "Saturn"
        Uranus  -> "Uranus"
        Neptune -> "Neptune"
        Pluto   -> "Pluto"

stringToSampleItem : String -> Maybe SampleItem
stringToSampleItem str =
    case str of
        "Mercury" -> Just Mercury
        "Venus"   -> Just Venus
        "Earth"   -> Just Earth
        "Mars"    -> Just Mars
        "Jupiter" -> Just Jupiter
        "Saturn"  -> Just Saturn
        "Uranus"  -> Just Uranus
        "Neptune" -> Just Neptune
        "Pluto"   -> Just Pluto
        _         -> Nothing

-- Tooltip ID type for managing tooltip state
type TooltipId = EarthGradingTooltip 

-- Function to create options with tooltips, taking the current state
exampleOptionItems : State -> List (SelectElement.OptionItem SampleItem Msg)
exampleOptionItems state =
    [ SelectElement.Group "Inner Planets"
        [ { content = Html.Styled.text "Mercury"
          , triggerLabel = "Mercury"
          , value = Mercury
          }
        , { content = Html.Styled.text "Venus"
          , triggerLabel = "Venus"
          , value = Venus
          }
        , { content = 
                div [ css [ Css.displayFlex, Css.alignItems Css.center ] ]
                    [ span [ css [ Css.marginRight (Css.px 8) ] ]
                        [ Tooltip.view
                            { id = "earth-tooltip" 
                            , trigger = \\attrs -> 
                                span 
                                    (css [ Css.cursor Css.pointer ] :: attrs) 
                                    [ UiIcon.gradingAssistant
                                        |> Svg.withColor Colors.azure
                                        |> Svg.withWidth (Css.px 16)
                                        |> Svg.withHeight (Css.px 16)
                                        |> Svg.toHtml
                                    ]
                            }
                            [ Tooltip.plaintext "Mostly harmless. Occasionally requires adult supervision."
                            , Tooltip.onToggle (ToggleTooltip EarthGradingTooltip)
                            , Tooltip.open (state.openTooltip == Just EarthGradingTooltip)
                            , Tooltip.smallPadding
                            , Tooltip.onRight
                            , Tooltip.disclosure { triggerId = "earth-tooltip", lastId = Nothing }
                            ]
                      ]
                    , text "Earth" 
                    ]
          , triggerLabel = "Earth"
          , value = Earth 
          }
        , { content = Html.Styled.text "Mars"
          , triggerLabel = "Mars"
          , value = Mars
          }
        ]
    , SelectElement.Group "Gas Giants"
        [ { content = Html.Styled.text "Jupiter"
          , triggerLabel = "Jupiter"
          , value = Jupiter
          }
        , { content = Html.Styled.text "Saturn"
          , triggerLabel = "Saturn"
          , value = Saturn
          }
        , { content = Html.Styled.text "Uranus"
          , triggerLabel = "Uranus"
          , value = Uranus
          }
        , { content = Html.Styled.text "Neptune"
          , triggerLabel = "Neptune"
          , value = Neptune
          }
        ]
    , SelectElement.Item
        { content = Html.Styled.text "Pluto"
        , triggerLabel = "Pluto"
        , value = Pluto
        }
    ]
"""
            ]
        , renderExample = Code.unstyledView
        , toExampleCode =
            \currentSettings ->
                let
                    selectedValCode =
                        case state.currentSelection of
                            Just s ->
                                "Just " ++ sampleItemToString s

                            Nothing ->
                                "Nothing"

                    titleAttrCode =
                        if currentSettings.title then
                            [ "SelectElement.title \"My Customizable Select (Title)\"" ]

                        else
                            []

                    labelAttrCode =
                        [ "SelectElement.label \"My Dynamic Select\""
                        ]
                            ++ (if currentSettings.hideLabel then
                                    [ "SelectElement.hideLabel " ++ Code.bool currentSettings.hideLabel ]

                                else
                                    []
                               )

                    stateAttrCode =
                        (if currentSettings.disabled then
                            [ "SelectElement.disabled " ++ Code.bool currentSettings.disabled ]

                         else
                            []
                        )
                            ++ (if currentSettings.loading then
                                    [ "SelectElement.loading " ++ Code.bool currentSettings.loading ]

                                else
                                    []
                               )

                    disableWhenCode =
                        if currentSettings.disableAllOptions then
                            [ "SelectElement.disableWhen (\\item -> List.member item [ Jupiter, Saturn ] || " ++ Code.bool currentSettings.disableAllOptions ++ ")" ]

                        else
                            [ "SelectElement.disableWhen (\\item -> List.member item [ Jupiter, Saturn ])" ]

                    iconCode =
                        if currentSettings.icon then
                            [ "SelectElement.icon UiIcon.checkmark" ]

                        else
                            []

                    errorGuidanceCode =
                        (if not (String.isEmpty currentSettings.error) then
                            [ "SelectElement.error " ++ Code.string currentSettings.error ]

                         else
                            []
                        )
                            ++ (if not (String.isEmpty currentSettings.guidance) && String.isEmpty currentSettings.error then
                                    [ "SelectElement.guidance " ++ Code.string currentSettings.guidance ]

                                else
                                    []
                               )

                    layoutCode =
                        (if currentSettings.containerCss then
                            [ "SelectElement.containerCss [ Css.backgroundColor (Css.hex \"FFFACD\") ]" ]

                         else
                            []
                        )
                            ++ (if currentSettings.noMargin then
                                    [ "SelectElement.noMargin " ++ Code.bool currentSettings.noMargin ]

                                else
                                    []
                               )

                    attrCodeList =
                        [ "SelectElement.optionItems (exampleOptionItems state)"
                        , "SelectElement.valueToString sampleItemToString"
                        , "SelectElement.stringToValue stringToSampleItem"
                        ]
                            ++ (case state.currentSelection of
                                    Just _ ->
                                        [ "SelectElement.selectedValue (" ++ selectedValCode ++ ")" ]

                                    Nothing ->
                                        []
                               )
                            ++ [ "SelectElement.onSelect OptionSelectedMsg"
                               , "SelectElement.triggerId " ++ Code.string currentSettings.triggerId
                               , "SelectElement.popoverId " ++ Code.string currentSettings.popoverId
                               ]
                            ++ titleAttrCode
                            ++ labelAttrCode
                            ++ stateAttrCode
                            ++ disableWhenCode
                            ++ iconCode
                            ++ errorGuidanceCode
                            ++ layoutCode
                in
                [ { sectionName = "Example"
                  , code =
                        Code.fromModule moduleName "view "
                            ++ Code.string "Select an option..."
                            ++ Code.listMultiline attrCodeList 1
                  }
                ]
        }
    , Heading.h2
        [ Heading.plaintext "Customizable Example"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx, Css.marginBottom (Css.px 16) ]
        ]
    , div [ Attributes.css [ Css.maxWidth (Css.px 400) ] ]
        [ SelectElement.view "Select an option..." attrs
        , p [] [ text ("Selected: " ++ Maybe.withDefault "None" (Maybe.map sampleItemToString state.currentSelection)) ]
        ]
    ]


type alias Model =
    { options : List (SelectElement.OptionItem SampleItem Msg)
    , triggerId : String
    , popoverId : String
    , title : Bool
    , hideLabel : Bool
    , disabled : Bool
    , loading : Bool
    , icon : Bool
    , disableAllOptions : Bool
    , error : String
    , guidance : String
    , containerCss : Bool
    , noMargin : Bool
    }


{-| -}
type alias State =
    { control : Control Model
    , currentSelection : Maybe SampleItem
    , openTooltip : Maybe TooltipId
    }


{-| -}
init : State
init =
    let
        placeholderOptions =
            [ SelectElement.Group "Inner Planets"
                [ { content = Html.Styled.text (sampleItemToLabel Mercury)
                  , triggerLabel = sampleItemToLabel Mercury
                  , value = Mercury
                  }
                , { content = Html.Styled.text (sampleItemToLabel Venus)
                  , triggerLabel = sampleItemToLabel Venus
                  , value = Venus
                  }
                , { content = Html.Styled.text (sampleItemToLabel Earth)
                  , triggerLabel = sampleItemToLabel Earth
                  , value = Earth
                  }
                , { content = Html.Styled.text (sampleItemToLabel Mars)
                  , triggerLabel = sampleItemToLabel Mars
                  , value = Mars
                  }
                ]
            , SelectElement.Group "Gas Giants"
                [ { content = Html.Styled.text (sampleItemToLabel Jupiter)
                  , triggerLabel = sampleItemToLabel Jupiter
                  , value = Jupiter
                  }
                , { content = Html.Styled.text (sampleItemToLabel Saturn)
                  , triggerLabel = sampleItemToLabel Saturn
                  , value = Saturn
                  }
                , { content = Html.Styled.text (sampleItemToLabel Uranus)
                  , triggerLabel = sampleItemToLabel Uranus
                  , value = Uranus
                  }
                , { content = Html.Styled.text (sampleItemToLabel Neptune)
                  , triggerLabel = sampleItemToLabel Neptune
                  , value = Neptune
                  }
                ]
            , SelectElement.Item
                { content = Html.Styled.text (sampleItemToLabel Pluto)
                , triggerLabel = sampleItemToLabel Pluto
                , value = Pluto
                }
            ]

        initialModel =
            { options = placeholderOptions
            , triggerId = "custom-select-trigger-1"
            , popoverId = "custom-select-popover-1"
            , title = True
            , hideLabel = False
            , disabled = False
            , loading = False
            , icon = False
            , disableAllOptions = False
            , error = ""
            , guidance = "This is some helpful guidance."
            , containerCss = False
            , noMargin = False
            }
    in
    { control =
        Control.record Model
            |> Control.field "options" (Control.value placeholderOptions)
            |> Control.field "triggerId" (Control.string initialModel.triggerId)
            |> Control.field "popoverId" (Control.string initialModel.popoverId)
            |> Control.field "title" (Control.bool initialModel.title)
            |> Control.field "hideLabel" (Control.bool initialModel.hideLabel)
            |> Control.field "disabled" (Control.bool initialModel.disabled)
            |> Control.field "loading" (Control.bool initialModel.loading)
            |> Control.field "icon" (Control.bool initialModel.icon)
            |> Control.field "disable all options (using disableWhen)" (Control.bool initialModel.disableAllOptions)
            |> Control.field "error" (Control.string initialModel.error)
            |> Control.field "guidance" (Control.string initialModel.guidance)
            |> Control.field "containerCss" (Control.bool initialModel.containerCss)
            |> Control.field "noMargin" (Control.bool initialModel.noMargin)
    , currentSelection = firstSelectableSampleItem placeholderOptions
    , openTooltip = Nothing
    }


{-| -}
type Msg
    = UpdateControl (Control Model)
    | OptionSelectedMsg SampleItem
    | ToggleTooltip TooltipId Bool


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl newControl ->
            ( { state | control = newControl }, Cmd.none )

        OptionSelectedMsg newSelection ->
            ( { state | currentSelection = Just newSelection }, Cmd.none )

        ToggleTooltip tooltipId isOpen ->
            ( { state
                | openTooltip =
                    if isOpen then
                        Just tooltipId

                    else
                        Nothing
              }
            , Cmd.none
            )
