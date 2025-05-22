module Spec.Nri.Ui.SelectElement exposing (spec)

import Accessibility.Aria as Aria
import Expect
import Html.Attributes as UnstyledAttributes
import Html.Styled
import Html.Styled.Attributes as Attributes
import Json.Encode
import Nri.Ui.SelectElement.V1 as SelectElement
import Nri.Ui.UiIcon.V2 as UiIcon
import ProgramTest exposing (..)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)



-- SAMPLE DATA


type SampleItem
    = Alpha
    | Beta
    | Gamma
    | Delta


sampleItemToString : SampleItem -> String
sampleItemToString item =
    case item of
        Alpha ->
            "Alpha"

        Beta ->
            "Beta"

        Gamma ->
            "Gamma"

        Delta ->
            "Delta"


stringToSampleItem : String -> Maybe SampleItem
stringToSampleItem str =
    case str of
        "Alpha" ->
            Just Alpha

        "Beta" ->
            Just Beta

        "Gamma" ->
            Just Gamma

        "Delta" ->
            Just Delta

        _ ->
            Nothing


testOptions : List (SelectElement.OptionItem SampleItem Msg)
testOptions =
    [ SelectElement.Item
        { content = Html.Styled.text "Alpha Item Content"
        , value = Alpha
        , triggerLabel = "Alpha Trigger Label"
        }
    , SelectElement.Item
        { content = Html.Styled.text "Beta Item Content"
        , value = Beta
        , triggerLabel = "Beta Trigger Label"
        }
    ]


testOptionsWithGroup : List (SelectElement.OptionItem SampleItem Msg)
testOptionsWithGroup =
    [ SelectElement.Group "Group One"
        [ { content = Html.Styled.text "Alpha Item Content"
          , value = Alpha
          , triggerLabel = "Alpha Trigger Label"
          }
        , { content = Html.Styled.text "Beta Item Content"
          , value = Beta
          , triggerLabel = "Beta Trigger Label"
          }
        ]
    , SelectElement.Item
        { content = Html.Styled.text "Gamma Item Content"
        , value = Gamma
        , triggerLabel = "Gamma Trigger Label"
        }
    ]



-- MODEL, MSG, UPDATE for tests


type alias Model =
    { selectedValue : Maybe SampleItem
    , lastMsgReceived : Maybe Msg
    }


initialModel : Model
initialModel =
    { selectedValue = Nothing
    , lastMsgReceived = Nothing
    }


type Msg
    = ItemSelected SampleItem


update : Msg -> Model -> Model
update msg model =
    case msg of
        ItemSelected item ->
            { model | selectedValue = Just item, lastMsgReceived = Just msg }



-- THE SPEC


spec : Test
spec =
    describe "Nri.Ui.SelectElement.V1"
        [ test "renders with default trigger text and options" <|
            \() ->
                ProgramTest.createSandbox
                    { init = initialModel
                    , update = update
                    , view =
                        \_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptions
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.onSelect ItemSelected
                                , SelectElement.triggerId "trigger-1"
                                , SelectElement.popoverId "popover-1"
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "button"
                        , id "trigger-1"
                        , containing [ text "Pick One" ]
                        , attribute (UnstyledAttributes.attribute "popovertarget" "popover-1")
                        , attribute (UnstyledAttributes.attribute "aria-haspopup" "listbox")
                        ]
                    |> ensureViewHas
                        [ tag "select-element"
                        , id "popover-1"
                        , attribute (UnstyledAttributes.attribute "data-trigger-id" "trigger-1")
                        , attribute (UnstyledAttributes.attribute "popover" "auto")
                        , containing [ tag "div", attribute (UnstyledAttributes.attribute "role" "option"), attribute (UnstyledAttributes.attribute "data-value" "Alpha"), containing [ text "Alpha Item Content" ] ]
                        , containing [ tag "div", attribute (UnstyledAttributes.attribute "role" "option"), attribute (UnstyledAttributes.attribute "data-value" "Beta"), containing [ text "Beta Item Content" ] ]
                        ]
                    |> done
        , test "renders selected value's triggerLabel on button and value on select-element" <|
            \() ->
                ProgramTest.createSandbox
                    { init = { initialModel | selectedValue = Just Alpha }
                    , update = update
                    , view =
                        \model_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptions
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.onSelect ItemSelected
                                , SelectElement.selectedValue model_.selectedValue
                                , SelectElement.triggerId "trigger-selected"
                                , SelectElement.popoverId "popover-selected"
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "button"
                        , id "trigger-selected"
                        , containing [ text "Alpha Trigger Label" ]
                        ]
                    |> ensureViewHas
                        [ tag "select-element"
                        , id "popover-selected"
                        , attribute (UnstyledAttributes.attribute "value" "Alpha")
                        ]
                    |> done
        , test "handles onSelect when 'select-change' event occurs" <|
            \() ->
                let
                    eventPayload =
                        Json.Encode.object
                            [ ( "detail"
                              , Json.Encode.object [ ( "value", Json.Encode.string "Beta" ) ]
                              )
                            ]

                    program =
                        ProgramTest.createSandbox
                            { init = initialModel
                            , update = update
                            , view =
                                \_ ->
                                    SelectElement.view "Pick One"
                                        [ SelectElement.optionItems testOptions
                                        , SelectElement.valueToString sampleItemToString
                                        , SelectElement.stringToValue stringToSampleItem
                                        , SelectElement.onSelect ItemSelected
                                        , SelectElement.triggerId "trigger-event"
                                        , SelectElement.popoverId "popover-event"
                                        ]
                                        |> Html.Styled.toUnstyled
                            }
                            |> ProgramTest.start ()
                in
                program
                    |> ProgramTest.simulateDomEvent
                        (Query.find [ tag "select-element", id "popover-event" ])
                        (Event.custom "select-change" eventPayload)
                    |> ProgramTest.expectModel
                        (\model_ ->
                            Expect.equal ( model_.selectedValue, model_.lastMsgReceived ) ( Just Beta, Just (ItemSelected Beta) )
                        )
        , test "is disabled when disabled attribute is true" <|
            \() ->
                ProgramTest.createSandbox
                    { init = initialModel
                    , update = update
                    , view =
                        \_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptions
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.triggerId "trigger-disabled"
                                , SelectElement.popoverId "popover-disabled"
                                , SelectElement.disabled True
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "button"
                        , id "trigger-disabled"
                        , attribute (UnstyledAttributes.disabled True)
                        , attribute (Aria.disabled True)
                        ]
                    |> done
        , test "is in loading state (and disabled) when loading attribute is true" <|
            \() ->
                ProgramTest.createSandbox
                    { init = initialModel
                    , update = update
                    , view =
                        \_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptions
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.triggerId "trigger-loading"
                                , SelectElement.popoverId "popover-loading"
                                , SelectElement.loading True
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "button"
                        , id "trigger-loading"
                        , attribute (UnstyledAttributes.disabled True)
                        , attribute (Aria.disabled True)
                        ]
                    |> done
        , test "disables individual options based on disableWhen" <|
            \() ->
                ProgramTest.createSandbox
                    { init = initialModel
                    , update = update
                    , view =
                        \_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptions
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.triggerId "trigger-dw"
                                , SelectElement.popoverId "popover-dw"
                                , SelectElement.disableWhen (\item -> item == Alpha)
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "select-element"
                        , id "popover-dw"
                        , containing [ tag "div", attribute (UnstyledAttributes.attribute "data-value" "Alpha"), attribute (UnstyledAttributes.attribute "aria-disabled" "true") ]
                        ]
                    |> ensureViewHasNot
                        [ tag "select-element"
                        , id "popover-dw"
                        , containing [ tag "div", attribute (UnstyledAttributes.attribute "data-value" "Beta"), attribute (UnstyledAttributes.attribute "aria-disabled" "true") ]
                        ]
                    |> done
        , test "renders an icon when provided" <|
            \() ->
                let
                    dummyIcon =
                        UiIcon.checkmark
                in
                ProgramTest.createSandbox
                    { init = initialModel
                    , update = update
                    , view =
                        \_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptions
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.triggerId "trigger-icon"
                                , SelectElement.popoverId "popover-icon"
                                , SelectElement.icon dummyIcon
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "button"
                        , id "trigger-icon"
                        , containing [ tag "span" ]
                        ]
                    |> done
        , test "renders a title when provided" <|
            \() ->
                ProgramTest.createSandbox
                    { init = initialModel
                    , update = update
                    , view =
                        \_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptions
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.triggerId "trigger-title"
                                , SelectElement.popoverId "popover-title"
                                , SelectElement.title "My Select Title"
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "h3"
                        , containing [ text "My Select Title" ]
                        ]
                    |> done
        , test "renders a visible label when provided" <|
            \() ->
                ProgramTest.createSandbox
                    { init = initialModel
                    , update = update
                    , view =
                        \_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptions
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.triggerId "trigger-label"
                                , SelectElement.popoverId "popover-label"
                                , SelectElement.label "My Awesome Label"
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "label"
                        , attribute (UnstyledAttributes.for "trigger-label")
                        , containing [ text "My Awesome Label" ]
                        ]
                    |> done
        , test "renders a hidden label when hideLabel is true" <|
            \() ->
                ProgramTest.createSandbox
                    { init = initialModel
                    , update = update
                    , view =
                        \_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptions
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.triggerId "trigger-hidden-label"
                                , SelectElement.popoverId "popover-hidden-label"
                                , SelectElement.label "My Hidden Label"
                                , SelectElement.hideLabel True
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "label"
                        , attribute (UnstyledAttributes.for "trigger-hidden-label")
                        , containing [ text "My Hidden Label" ]
                        ]
                    |> done
        , test "renders an error message" <|
            \() ->
                ProgramTest.createSandbox
                    { init = initialModel
                    , update = update
                    , view =
                        \_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptions
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.triggerId "trigger-err"
                                , SelectElement.popoverId "popover-err"
                                , SelectElement.error "This is an error!"
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "div"
                        , containing [ text "This is an error!" ]
                        ]
                    |> done
        , test "renders a guidance message" <|
            \() ->
                ProgramTest.createSandbox
                    { init = initialModel
                    , update = update
                    , view =
                        \_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptions
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.triggerId "trigger-guide"
                                , SelectElement.popoverId "popover-guide"
                                , SelectElement.guidance "This is helpful guidance."
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "div"
                        , containing [ text "This is helpful guidance." ]
                        ]
                    |> done
        , test "error message overrides guidance message" <|
            \() ->
                ProgramTest.createSandbox
                    { init = initialModel
                    , update = update
                    , view =
                        \_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptions
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.triggerId "trigger-override"
                                , SelectElement.popoverId "popover-override"
                                , SelectElement.guidance "This guidance should be hidden."
                                , SelectElement.error "This error should show."
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "div"
                        , containing [ text "This error should show." ]
                        ]
                    |> ensureViewHasNot
                        [ containing [ text "This guidance should be hidden." ] ]
                    |> done
        , test "applies custom HTML attributes to the trigger" <|
            \() ->
                ProgramTest.createSandbox
                    { init = initialModel
                    , update = update
                    , view =
                        \_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptions
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.triggerId "trigger-custom-attr"
                                , SelectElement.popoverId "popover-custom-attr"
                                , SelectElement.customHtmlAttributesForTrigger [ Attributes.attribute "data-custom-trigger" "trigger-val" ]
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "button"
                        , id "trigger-custom-attr"
                        , attribute (UnstyledAttributes.attribute "data-custom-trigger" "trigger-val")
                        ]
                    |> done
        , test "applies custom HTML attributes to the popover" <|
            \() ->
                ProgramTest.createSandbox
                    { init = initialModel
                    , update = update
                    , view =
                        \_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptions
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.triggerId "trigger-custom-pop"
                                , SelectElement.popoverId "popover-custom-pop"
                                , SelectElement.customHtmlAttributesForPopover [ Attributes.attribute "data-custom-popover" "popover-val" ]
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "select-element"
                        , id "popover-custom-pop"
                        , attribute (UnstyledAttributes.attribute "data-custom-popover" "popover-val")
                        ]
                    |> done
        , test "renders grouped options correctly" <|
            \() ->
                ProgramTest.createSandbox
                    { init = initialModel
                    , update = update
                    , view =
                        \_ ->
                            SelectElement.view "Pick One"
                                [ SelectElement.optionItems testOptionsWithGroup
                                , SelectElement.valueToString sampleItemToString
                                , SelectElement.stringToValue stringToSampleItem
                                , SelectElement.triggerId "trigger-group"
                                , SelectElement.popoverId "popover-group"
                                ]
                                |> Html.Styled.toUnstyled
                    }
                    |> ProgramTest.start ()
                    |> ensureViewHas
                        [ tag "select-element"
                        , id "popover-group"
                        , containing [ tag "div", attribute (UnstyledAttributes.attribute "role" "group"), attribute (UnstyledAttributes.attribute "aria-label" "Group One") ]
                        , containing [ tag "div", attribute (UnstyledAttributes.attribute "role" "option"), attribute (UnstyledAttributes.attribute "data-value" "Alpha"), containing [ text "Alpha Item Content" ] ]
                        , containing [ tag "div", attribute (UnstyledAttributes.attribute "role" "option"), attribute (UnstyledAttributes.attribute "data-value" "Beta"), containing [ text "Beta Item Content" ] ]
                        , containing [ tag "div", attribute (UnstyledAttributes.attribute "role" "option"), attribute (UnstyledAttributes.attribute "data-value" "Gamma"), containing [ text "Gamma Item Content" ] ]
                        ]
                    |> done
        ]
