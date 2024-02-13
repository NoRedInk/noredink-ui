module Examples.Checkbox exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import CheckboxIcons
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Guidance
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Checkbox.V7 as Checkbox
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Html.Attributes.V2 exposing (safeIdWithPrefix)
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Table.V7 as Table
import Nri.Ui.Tooltip.V3 as Tooltip


moduleName : String
moduleName =
    "Checkbox"


version : Int
version =
    7


type TooltipType
    = HelpfullyDisabled


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = preview
    , about = Guidance.communicateState moduleName ++ [ Guidance.helpfullyDisabled moduleName ] ++ Guidance.message moduleName
    , view =
        \ellieLinkConfig state ->
            let
                settings =
                    Control.currentValue state.settings

                ( exampleCode, exampleView ) =
                    viewExampleWithCode state settings
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateControls
                , settings = state.settings
                , mainType = Just "RootHtml.Html Bool"
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode = \_ -> [ { sectionName = "Example", code = exampleCode } ]
                }
            , Heading.h2
                [ Heading.plaintext "Customizable example"
                , Heading.css [ Css.marginTop (Css.px 30) ]
                ]
            , exampleView
            , Heading.h2
                [ Heading.plaintext "State Examples"
                , Heading.css [ Css.marginTop (Css.px 30) ]
                ]
            , Table.view []
                [ Table.string
                    { header = "State"
                    , value = .state
                    , width = Css.pct 30
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle, Css.fontWeight Css.bold ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Enabled"
                    , view = .enabled
                    , width = Css.px 150
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Disabled"
                    , view = .disabled
                    , width = Css.px 150
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
                    , sort = Nothing
                    }
                ]
                (List.indexedMap row
                    [ ( "NotSelected", Checkbox.NotSelected )
                    , ( "PartiallySelected", Checkbox.PartiallySelected )
                    , ( "Selected", Checkbox.Selected )
                    ]
                )
            , Heading.h2
                [ Heading.plaintext "Guidance Examples"
                , Heading.css [ Css.marginTop (Css.px 30) ]
                ]
            , Table.view []
                [ Table.custom
                    { header = text "Attribute"
                    , view = .name >> text
                    , width = Css.pct 10
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Example"
                    , view =
                        \{ name, attribute } ->
                            Checkbox.view
                                { label = "I agree to the terms and conditions"
                                , selected = Checkbox.NotSelected
                                }
                                [ Checkbox.id (safeIdWithPrefix "guidance-and-error-example" name)
                                , attribute
                                ]
                    , width = Css.pct 50
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.middle ]
                    , sort = Nothing
                    }
                ]
                [ { name = "guidance"
                  , attribute = Checkbox.guidance "Be sure to read the terms and conditions before agreeing to follow them."
                  }
                , { name = "guidanceHtml"
                  , attribute =
                        Checkbox.guidanceHtml
                            [ text "Be sure to read "
                            , ClickableText.link "the terms and conditions"
                                [ ClickableText.linkExternal "https://en.wikipedia.org/wiki/Terms_of_service"
                                , ClickableText.small
                                , ClickableText.appearsInline
                                ]
                            , text " before agreeing to follow them."
                            ]
                  }
                ]
            , Heading.h2
                [ Heading.plaintext "Helpfully Disabled Example"
                , Heading.css [ Css.marginTop (Css.px 30) ]
                ]
            , Tooltip.view
                { trigger =
                    \attrs ->
                        Checkbox.view
                            { label = "Enable Text-to-Speech"
                            , selected = Checkbox.NotSelected
                            }
                            [ Checkbox.id "tooltip-example"
                            , Checkbox.disabled
                            , Checkbox.custom attrs
                            ]
                , id = "tooltip"
                }
                [ Tooltip.helpfullyDisabled
                , Tooltip.open (state.openTooltip == Just HelpfullyDisabled)
                , Tooltip.onToggle (ToggleTooltip HelpfullyDisabled)
                , Tooltip.paragraph "Reasons why you can't enable Text-to-Speech"
                , Tooltip.onRight
                , Tooltip.fitToContent
                ]
            ]
    , categories = [ Inputs ]
    , keyboardSupport =
        [ { keys = [ Space ]
          , result = "Select or deselect the checkbox"
          }
        ]
    }


row :
    Int
    -> ( String, Checkbox.IsSelected )
    -> { state : String, enabled : Html Msg, disabled : Html Msg }
row i ( name, selectionStatus ) =
    { state = name
    , enabled =
        Checkbox.view
            { label = "Setting"
            , selected = selectionStatus
            }
            [ Checkbox.id <| "enabled-" ++ String.fromInt i
            , Checkbox.onCheck <| \_ -> Swallow
            , Checkbox.enabled
            , Checkbox.visibleLabel
            ]
    , disabled =
        Checkbox.view
            { label = "Setting"
            , selected = selectionStatus
            }
            [ Checkbox.id <| "disabled-" ++ String.fromInt i
            , Checkbox.onCheck <| \_ -> Swallow
            , Checkbox.disabled
            , Checkbox.visibleLabel
            ]
    }


preview : List (Html Never)
preview =
    let
        renderPreview ( icon, label_ ) =
            span
                [ css
                    [ Css.color Colors.navy
                    , Css.fontSize (Css.px 15)
                    , Fonts.baseFont
                    , Css.fontWeight (Css.int 600)
                    , Css.displayFlex
                    , Css.alignItems Css.center
                    , Css.minHeight (Css.px 30)
                    ]
                ]
                [ Svg.toHtml (Svg.withCss [ Css.marginRight (Css.px 8) ] icon)
                , text label_
                ]
    in
    [ ( CheckboxIcons.checkedPartially "checkedPartially-preview-checkedPartially", "Part checked" )
    , ( CheckboxIcons.unchecked "unchecked-preview-unchecked", "Unchecked" )
    , ( CheckboxIcons.checked "checkbox-preview-checked", "Checked" )
    ]
        |> List.map renderPreview


{-| -}
type alias State =
    { isChecked : Checkbox.IsSelected
    , settings : Control Settings
    , openTooltip : Maybe TooltipType
    }


{-| -}
init : State
init =
    { isChecked = Checkbox.PartiallySelected
    , settings = controlSettings
    , openTooltip = Nothing
    }


type alias Settings =
    { label : String
    , attributes : List ( String, Checkbox.Attribute Msg )
    }


controlSettings : Control Settings
controlSettings =
    Control.record Settings
        |> Control.field "label" (Control.string "Enable Text-to-Speech")
        |> Control.field "" initAttributes


initAttributes : Control (List ( String, Checkbox.Attribute Msg ))
initAttributes =
    Control.list
        |> ControlExtra.optionalBoolListItem "disabled" ( "disabled", Checkbox.disabled )
        |> ControlExtra.listItems "Guidance"
            (Control.list
                |> CommonControls.guidanceAndErrorMessage
                    { moduleName = moduleName
                    , guidance = Checkbox.guidance
                    , guidanceHtml = Checkbox.guidanceHtml
                    , errorMessage = Nothing
                    , message = "There is something you need to be aware of."
                    }
            )
        |> ControlExtra.listItems "CSS & Extra Styles"
            (Control.list
                |> ControlExtra.optionalListItem "containerCss"
                    (Control.choice
                        [ ( "Red dashed border"
                          , Control.value
                                ( "Checkbox.containerCss [ Css.border3 (Css.px 4) Css.dashed Colors.red ]"
                                , Checkbox.containerCss [ Css.border3 (Css.px 4) Css.dashed Colors.red ]
                                )
                          )
                        ]
                    )
                |> ControlExtra.optionalListItem "labelCss"
                    (Control.choice
                        [ ( "Orange dotted border"
                          , Control.value
                                ( "Checkbox.labelCss [ Css.border3 (Css.px 4) Css.dotted Colors.orange ]"
                                , Checkbox.labelCss [ Css.border3 (Css.px 4) Css.dotted Colors.orange ]
                                )
                          )
                        ]
                    )
                |> ControlExtra.optionalBoolListItem "noMargin" ( "noMargin True", Checkbox.noMargin True )
                |> ControlExtra.optionalBoolListItem "hiddenLabel" ( "hiddenLabel", Checkbox.hiddenLabel )
            )


viewExampleWithCode : State -> Settings -> ( String, Html Msg )
viewExampleWithCode state settings =
    let
        id =
            "unique-example-id"
    in
    ( [ Code.fromModule moduleName "view"
      , Code.recordMultiline
            [ ( "label", Code.string settings.label )
            , ( "selected", Code.fromModule "Checkbox" (Debug.toString state.isChecked) )
            ]
            1
      , Code.listMultiline
            ([ Code.fromModule moduleName "id " ++ Code.string id
             , Code.fromModule moduleName "onCheck " ++ "identity"
             ]
                ++ List.map Tuple.first settings.attributes
            )
            1
      ]
        |> String.join ""
    , Checkbox.view
        { label = settings.label
        , selected = state.isChecked
        }
        ([ Checkbox.id id
         , Checkbox.onCheck (ToggleCheck id)
         ]
            ++ List.map Tuple.second settings.attributes
        )
    )


{-| -}
type Msg
    = ToggleCheck Id Bool
    | UpdateControls (Control Settings)
    | Swallow
    | ToggleTooltip TooltipType Bool


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ToggleCheck id checked ->
            let
                isChecked =
                    if checked then
                        Checkbox.Selected

                    else
                        Checkbox.NotSelected
            in
            ( { state | isChecked = isChecked }, Cmd.none )

        UpdateControls settings ->
            ( { state | settings = settings }, Cmd.none )

        Swallow ->
            ( state, Cmd.none )

        ToggleTooltip type_ isOpen ->
            if isOpen then
                ( { state | openTooltip = Just type_ }, Cmd.none )

            else
                ( { state | openTooltip = Nothing }, Cmd.none )


type alias Id =
    String
