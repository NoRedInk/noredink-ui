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
import Guidance
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
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


controlAttributes : Control (List ( String, RadioButtonDotless.Attribute ControlSelection Msg ))
controlAttributes =
    ControlExtra.list
        |> ControlExtra.listItem "textAlign" textAlignControl
        |> ControlExtra.listItem "width" widthControl
        |> ControlExtra.optionalListItem "containerCss"
            (Control.choice
                [ ( "max-width with border"
                  , Control.value
                        ( "RadioButtonDotless.containerCss [ Css.maxWidth (Css.px 200), Css.border3 (Css.px 1) Css.solid Colors.red ]"
                        , RadioButtonDotless.containerCss [ Css.maxWidth (Css.px 200), Css.border3 (Css.px 1) Css.solid Colors.red ]
                        )
                  )
                , ( "10px right margin"
                  , Control.value
                        ( "RadioButtonDotless.containerCss [ Css.marginRight (Css.px 10) ]"
                        , RadioButtonDotless.containerCss [ Css.marginRight (Css.px 10) ]
                        )
                  )
                ]
            )
        |> ControlExtra.optionalListItem "labelCss"
            (Control.choice
                [ ( "backgroundColor highlightMagenta"
                  , Control.value
                        ( "RadioButtonDotless.labelCss [ Css.backgroundColor Colors.highlightMagenta ]"
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
            []
        , RadioButtonDotless.view
            { label = "Selected"
            , name = "radio-button-dotless"
            , value = 2
            , valueToString = String.fromInt
            , selectedValue = Just 2
            }
            []
        ]
    ]


view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig state =
    let
        selectionSettings =
            Control.currentValue state.selectionSettings
    in
    [ Heading.h2
        [ Heading.plaintext "Interactive example"
        , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
        ]
    , ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = SetSelectionSettings
        , settings = state.selectionSettings
        , mainType = Nothing
        , extraCode = []
        , renderExample = Code.unstyledView
        , toExampleCode =
            \_ ->
                [ { sectionName = "Example"
                  , code = ""
                  }
                ]
        }
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
            { header = text "Example"
            , view = .example
            , width = Css.px 500
            , cellStyles = always [ Css.padding (Css.px 10) ]
            , sort = Nothing
            }
        ]
        [ { description = "Short Buttons"
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
                                    , RadioButtonDotless.onSelect ShortRadioSelect
                                    ]
                            )
                    )
          }
        , { description = "Medium Buttons"
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
                                    , RadioButtonDotless.onSelect MediumRadioSelect
                                    ]
                            )
                    )
          }
        , { description = "Long Buttons"
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
                                    , RadioButtonDotless.onSelect LongRadioSelect
                                    ]
                            )
                    )
          }
        ]
    ]


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
