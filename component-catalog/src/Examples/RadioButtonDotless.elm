module Examples.RadioButtonDotless exposing
    ( Msg
    , State
    , example
    )

import Category exposing (Category(..))
import Code
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (css)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.RadioButtonDotless.V1 as RadioButtonDotless
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Table.V7 as Table
import Nri.Ui.Text.V6 as Text
import Platform.Sub as Sub


type alias State =
    { shortRadioValue : Maybe String
    , mediumRadioValue : Maybe String
    , longRadioValue : Maybe String
    , selectionSettings : Control SelectionSettings
    , selectedValue : Maybe ControlSelection
    }


type alias SelectionSettings =
    { dogsLabel : String
    , dogs : List ( String, RadioButtonDotless.Attribute ControlSelection Msg )
    , catsLabel : String
    , cats : List ( String, RadioButtonDotless.Attribute ControlSelection Msg )
    }


type ControlSelection
    = Dogs
    | Cats


selectionToString : SelectionSettings -> ControlSelection -> String
selectionToString selectionSettings selection =
    case selection of
        Dogs ->
            selectionSettings.dogsLabel

        Cats ->
            selectionSettings.catsLabel


type Msg
    = ShortRadioSelect String
    | MediumRadioSelect String
    | LongRadioSelect String
    | SetSelectionSettings (Control SelectionSettings)
    | Select ControlSelection
    | NoOp


controlAttributes : Control (List ( String, RadioButtonDotless.Attribute ControlSelection Msg ))
controlAttributes =
    ControlExtra.list
        |> ControlExtra.optionalListItem "textAlign" textAlignControl
        |> ControlExtra.optionalListItem "width" widthControl
        |> ControlExtra.optionalListItem "size" sizeControl
        |> ControlExtra.optionalListItem "enablement" enablementControl
        |> ControlExtra.optionalListItem "containerCss"
            (Control.choice
                [ ( "max-width with border"
                  , Control.value
                        ( "RadioButtonDotless.containerCss [ maxWidth (px 200), border3 (px 1) solid red ]"
                        , RadioButtonDotless.containerCss [ Css.maxWidth (Css.px 200), Css.border3 (Css.px 1) Css.solid Colors.red ]
                        )
                  )
                , ( "10px right margin"
                  , Control.value
                        ( "RadioButtonDotless.containerCss [ marginRight (px 10) ]"
                        , RadioButtonDotless.containerCss [ Css.marginRight (Css.px 10) ]
                        )
                  )
                ]
            )
        |> ControlExtra.optionalListItem "labelCss"
            (Control.choice
                [ ( "backgroundColor highlightMagenta"
                  , Control.value
                        ( "RadioButtonDotless.labelCss [ backgroundColor Colors.highlightMagenta ]"
                        , RadioButtonDotless.labelCss [ Css.backgroundColor Colors.highlightMagenta ]
                        )
                  )
                ]
            )


initSelectionSettings : Control SelectionSettings
initSelectionSettings =
    Control.record SelectionSettings
        |> Control.field "Dogs label" (Control.string "Dogs")
        |> Control.field "Dogs" controlAttributes
        |> Control.field "Cats label" (Control.string "Cats")
        |> Control.field "Cats" controlAttributes


textAlignControl : Control ( String, RadioButtonDotless.Attribute ControlSelection Msg )
textAlignControl =
    Control.choice
        [ ( "textAlignCenter", Control.value ( "RadioButtonDotless.textAlignCenter", RadioButtonDotless.textAlignCenter ) )
        , ( "textAlignLeft", Control.value ( "RadioButtonDotless.textAlignLeft", RadioButtonDotless.textAlignLeft ) )
        ]


widthControl : Control ( String, RadioButtonDotless.Attribute ControlSelection Msg )
widthControl =
    Control.choice
        [ ( "unboundedWidth", Control.value ( "RadioButtonDotless.unboundedWidth", RadioButtonDotless.unboundedWidth ) )
        , ( "fillContainerWidth", Control.value ( "RadioButtonDotless.fillContainerWidth", RadioButtonDotless.fillContainerWidth ) )
        ]


sizeControl : Control ( String, RadioButtonDotless.Attribute ControlSelection Msg )
sizeControl =
    Control.choice
        [ ( "small", Control.value ( "RadioButtonDotless.small", RadioButtonDotless.small ) )
        , ( "medium", Control.value ( "RadioButtonDotless.medium", RadioButtonDotless.medium ) )
        , ( "large", Control.value ( "RadioButtonDotless.large", RadioButtonDotless.large ) )
        ]


enablementControl : Control ( String, RadioButtonDotless.Attribute ControlSelection Msg )
enablementControl =
    Control.choice
        [ ( "enabled", Control.value ( "RadioButtonDotless.enabled", RadioButtonDotless.enabled ) )
        , ( "disabled", Control.value ( "RadioButtonDotless.disabled NoOp", RadioButtonDotless.disabled NoOp ) )
        ]


moduleName : String
moduleName =
    "RadioButtonDotless"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = preview
    , about =
        [ Text.mediumBodyGray [ Text.markdown "This component behaves like a `RadioButton`, but has the look and feel of a normal `Button`.  It is a good choice to use for \"multiple choice\" style questions as it separates the act of choosing an answer and submitting the answer - preventing frustrating mistakes." ]
        ]
    , view = view
    , categories = [ Inputs ]
    , keyboardSupport =
        [ { keys = [ Arrow Left ]
          , result = "Move the focus & select the previous radio button "
          }
        , { keys = [ Arrow Up ]
          , result = "Move the focus & select the previous radio button "
          }
        , { keys = [ Arrow Right ]
          , result = "Move the focus & select the next radio button"
          }
        , { keys = [ Arrow Down ]
          , result = "Move the focus & select the next radio button"
          }
        , { keys = [ Space ]
          , result = "Select the current radio button if none are selected"
          }
        ]
    }


init : State
init =
    { shortRadioValue = Nothing
    , mediumRadioValue = Nothing
    , longRadioValue = Nothing
    , selectionSettings = initSelectionSettings
    , selectedValue = Nothing
    }


update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ShortRadioSelect value ->
            ( { state | shortRadioValue = Just value }, Cmd.none )

        MediumRadioSelect value ->
            ( { state | mediumRadioValue = Just value }, Cmd.none )

        LongRadioSelect value ->
            ( { state | longRadioValue = Just value }, Cmd.none )

        SetSelectionSettings settings ->
            ( { state | selectionSettings = settings }, Cmd.none )

        Select selection ->
            ( { state | selectedValue = Just selection }, Cmd.none )

        NoOp ->
            ( state, Cmd.none )


preview : List (Html Never)
preview =
    [ div
        [ css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "5px"
            ]
        ]
        [ RadioButtonDotless.view
            { label = "Unselected"
            , name = "radio-button-dotless"
            , value = 1
            , valueToString = String.fromInt
            , selectedValue = Just 2
            }
            [ RadioButtonDotless.custom
                [ Attributes.tabindex 0
                , Attributes.attribute "aria-disabled" "true"
                , Attributes.disabled True
                ]
            ]
        , RadioButtonDotless.view
            { label = "Selected"
            , name = "radio-button-dotless"
            , value = 2
            , valueToString = String.fromInt
            , selectedValue = Just 2
            }
            [ RadioButtonDotless.custom
                [ Attributes.tabindex 0
                , Attributes.attribute "aria-disabled" "true"
                , Attributes.disabled True
                ]
            ]
        ]
    ]


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig state =
    let
        selectionSettings =
            Control.currentValue state.selectionSettings
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = SetSelectionSettings
        , settings = state.selectionSettings
        , mainType = Nothing
        , extraCode =
            [ Code.newlines
            , Code.unionType "Animals" [ "Dogs", "Cats" ]
            , Code.newlines
            , "toString : Animals -> String"
            , "toString animals ="
                ++ Code.caseExpression "animals"
                    [ ( "Dogs", Code.string selectionSettings.dogsLabel )
                    , ( "Cats", Code.string selectionSettings.catsLabel )
                    ]
                    1
            ]
        , renderExample = Code.unstyledView
        , toExampleCode =
            \_ ->
                [ { sectionName = "Example"
                  , code = viewExamplesCode selectionSettings state.selectedValue
                  }
                ]
        }
    , Heading.h2
        [ Heading.plaintext "Interactive example"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , viewExamples selectionSettings state.selectedValue
    , Heading.h2
        [ Heading.plaintext "Layout Configurations"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , Table.view
        []
        [ Table.custom
            { header = text "Description"
            , view = .description >> text
            , width = Css.px 100
            , cellStyles = always []
            , sort = Nothing
            }
        , Table.custom
            { header = text "Container Styles"
            , view = \{ containerStyles } -> code [] (List.intersperse (br [] []) <| List.map text containerStyles)
            , width = Css.px 200
            , cellStyles = always []
            , sort = Nothing
            }
        , Table.custom
            { header = text "RadioButtonDotless Attributes"
            , view = \{ attributes } -> code [] (List.intersperse (br [] []) <| List.map text attributes)
            , width = Css.px 200
            , cellStyles = always []
            , sort = Nothing
            }
        , Table.custom
            { header = text "Example"
            , view = .example
            , width = Css.px 500
            , cellStyles = always [ Css.padding (Css.px 10) ]
            , sort = Nothing
            }
        ]
        [ { description = "Short Buttons"
          , containerStyles =
                [ "displayFlex"
                , "flexDirection row"
                , "property \"gap\" \"10px\""
                ]
          , attributes =
                [ "fillContainerWidth"
                , "large"
                ]
          , example =
                div
                    [ css
                        [ Css.displayFlex
                        , Css.flexDirection Css.row
                        , Css.property "gap" "10px"
                        , Css.width (Css.px 550)
                        , Css.border3 (Css.px 1) Css.dashed Colors.navy
                        , Css.padding (Css.px 10)
                        ]
                    ]
                    ([ "crept", "past", "scary" ]
                        |> List.map
                            (\label ->
                                RadioButtonDotless.view
                                    { label = label
                                    , name = "radio-button-dotless-short"
                                    , value = label
                                    , valueToString = identity
                                    , selectedValue = state.shortRadioValue
                                    }
                                    [ RadioButtonDotless.fillContainerWidth
                                    , RadioButtonDotless.large
                                    , RadioButtonDotless.onSelect ShortRadioSelect
                                    ]
                            )
                    )
          }
        , { description = "Medium Buttons"
          , containerStyles =
                [ "displayFlex"
                , "flexDirection column"
                , "property \"gap\" \"10px\""
                ]
          , attributes =
                [ "fillContainerWidth"
                , "textAlignLeft"
                , "large"
                ]
          , example =
                div
                    [ css
                        [ Css.displayFlex
                        , Css.flexDirection Css.column
                        , Css.property "gap" "10px"
                        , Css.width (Css.px 550)
                        , Css.border3 (Css.px 1) Css.dashed Colors.navy
                        , Css.padding (Css.px 10)
                        ]
                    ]
                    ([ "The amount of homework eaten"
                     , "Who ate the homework"
                     , "The time the homework was eaten"
                     ]
                        |> List.map
                            (\label ->
                                RadioButtonDotless.view
                                    { label = label
                                    , name = "radio-button-dotless-medium"
                                    , value = String.replace " " "-" label
                                    , valueToString = identity
                                    , selectedValue = state.mediumRadioValue
                                    }
                                    [ RadioButtonDotless.fillContainerWidth
                                    , RadioButtonDotless.textAlignLeft
                                    , RadioButtonDotless.large
                                    , RadioButtonDotless.onSelect MediumRadioSelect
                                    ]
                            )
                    )
          }
        , { description = "Long Buttons"
          , containerStyles =
                [ "displayFlex"
                , "flexDirection column"
                , "property \"gap\" \"10px\""
                ]
          , attributes =
                [ "fillContainerWidth"
                , "textAlignLeft"
                , "large"
                ]
          , example =
                div
                    [ css
                        [ Css.displayFlex
                        , Css.flexDirection Css.column
                        , Css.property "gap" "10px"
                        , Css.width (Css.px 550)
                        , Css.border3 (Css.px 1) Css.dashed Colors.navy
                        , Css.padding (Css.px 10)
                        ]
                    ]
                    ([ "Future Perfect--an action will be completed before another future action"
                     , "Past Perfect--an action was completed before another past action"
                     , "Present Perfect--an action started in the past and continues, or it has just been completed"
                     ]
                        |> List.map
                            (\label ->
                                RadioButtonDotless.view
                                    { label = label
                                    , name = "radio-button-dotless-long"
                                    , value = String.replace " " "-" label
                                    , valueToString = identity
                                    , selectedValue = state.longRadioValue
                                    }
                                    [ RadioButtonDotless.fillContainerWidth
                                    , RadioButtonDotless.textAlignLeft
                                    , RadioButtonDotless.large
                                    , RadioButtonDotless.onSelect LongRadioSelect
                                    ]
                            )
                    )
          }
        ]
    ]


viewExamplesCode : SelectionSettings -> Maybe ControlSelection -> String
viewExamplesCode selectionSettings selectedValue =
    let
        toExampleCode ( kind, settings ) =
            Code.fromModule "RadioButtonDotless" "view"
                ++ Code.recordMultiline
                    [ ( "label", (selectionToString selectionSettings >> Code.string) kind )
                    , ( "name", Code.string "pets" )
                    , ( "value", selectionToString selectionSettings kind )
                    , ( "selectedValue"
                      , Code.maybe (Maybe.map (selectionToString selectionSettings) selectedValue)
                      )
                    , ( "valueToString", "toString" )
                    ]
                    2
                ++ Code.listMultiline (List.map Tuple.first settings) 2
    in
    "div []"
        ++ Code.listMultiline
            (List.map toExampleCode
                [ ( Dogs, selectionSettings.dogs )
                , ( Cats, selectionSettings.cats )
                ]
            )
            1


viewExamples : SelectionSettings -> Maybe ControlSelection -> Html Msg
viewExamples selectionSettings selectedValue =
    [ ( Dogs, selectionSettings.dogs )
    , ( Cats, selectionSettings.cats )
    ]
        |> List.map
            (\( kind, settings ) ->
                RadioButtonDotless.view
                    { label = selectionToString selectionSettings kind
                    , name = "pets"
                    , value = kind
                    , selectedValue = selectedValue
                    , valueToString = selectionToString selectionSettings
                    }
                    (RadioButtonDotless.onSelect Select :: List.map Tuple.second settings)
            )
        |> div [ css [ Css.marginTop (Css.px 30), Css.displayFlex, Css.flexDirection Css.row, Css.property "gap" "10px" ] ]
