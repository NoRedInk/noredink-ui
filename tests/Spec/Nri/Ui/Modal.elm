module Spec.Nri.Ui.Modal exposing (spec)

import Accessibility.Key as Key
import Expect
import Html.Attributes
import Html.Styled as Html exposing (Html, toUnstyled)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Encode as Encode
import Nri.Test.KeyboardHelpers.V1 exposing (pressTab, pressTabBack)
import Nri.Ui.Modal.V12 as Modal
import ProgramTest exposing (..)
import SimulatedEffect.Cmd
import Test exposing (..)
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Modal"
        [ test "titleId is attached to the modal title" <|
            \() ->
                start []
                    |> clickButton "Open Modal"
                    |> ensureViewHas
                        [ id Modal.titleId
                        , tag "h1"
                        , containing [ text modalTitle ]
                        , attribute (Key.tabbable False)
                        ]
                    |> done
        , focusTests
        , test "ATAC hole works" <|
            \() ->
                start [ Modal.atac (Html.text "ATAC content") ]
                    |> clickButton "Open Modal"
                    |> ensureViewHas [ text "ATAC content" ]
                    |> done
        ]


focusTests : Test
focusTests =
    describe "Focus management"
        [ test "focus starts on the specified element in the modal" <|
            \() ->
                start []
                    |> clickButton "Open Modal"
                    |> ensureFocused Modal.closeButtonId
                    |> done
        , test "focus returns to the element that opened the modal" <|
            \() ->
                start []
                    |> clickButton "Open Modal"
                    |> clickButton "Close modal"
                    |> ensureFocused "open-modal"
                    |> done
        , test "focus wraps from the modal title correctly" <|
            \() ->
                start []
                    |> clickButton "Open Modal"
                    |> tabBackWithinModal Modal.titleId
                    |> ensureFocused lastButtonId
                    |> done
        , test "focus wraps from the close button correctly" <|
            \() ->
                start []
                    |> clickButton "Open Modal"
                    |> tabBackWithinModal Modal.closeButtonId
                    |> ensureFocused lastButtonId
                    |> done
        , test "focus wraps from the last button correctly" <|
            \() ->
                start []
                    |> clickButton "Open Modal"
                    |> tabForwardWithinModal lastButtonId
                    |> ensureFocused Modal.closeButtonId
                    |> done
        ]


tabBackWithinModal : String -> ProgramTest a b c -> ProgramTest a b c
tabBackWithinModal onElementId =
    pressTabBack ProgramTest.simulateDomEvent { targetDetails = [ ( "id", Encode.string onElementId ) ] } focusTrapNode


tabForwardWithinModal : String -> ProgramTest a b c -> ProgramTest a b c
tabForwardWithinModal onElementId =
    pressTab ProgramTest.simulateDomEvent { targetDetails = [ ( "id", Encode.string onElementId ) ] } focusTrapNode


focusTrapNode : List Selector
focusTrapNode =
    [ attribute (Html.Attributes.attribute "data-testid" "focus-trap-node") ]


start : List Modal.Attribute -> ProgramTest Modal.Model Msg Effect
start modalAttributes =
    createElement
        { init = \_ -> ( Modal.init, None )
        , view = toUnstyled << view modalAttributes
        , update = update
        }
        |> withSimulatedEffects perform
        |> ProgramTest.start ()


type Msg
    = OpenModal String
    | ModalMsg Modal.Msg
    | Focus String


update : Msg -> Modal.Model -> ( Modal.Model, Effect )
update msg model =
    case msg of
        OpenModal returnFocusTo ->
            let
                ( newModel, focusOn ) =
                    Modal.open
                        { startFocusOn = Modal.closeButtonId
                        , returnFocusTo = returnFocusTo
                        }
            in
            ( newModel, FocusOn focusOn )

        ModalMsg modalMsg ->
            let
                ( newModel, maybeFocus ) =
                    Modal.update
                        { dismissOnEscAndOverlayClick = True }
                        modalMsg
                        model
            in
            ( newModel
            , Maybe.map FocusOn maybeFocus
                |> Maybe.withDefault None
            )

        Focus id ->
            ( model, FocusOn id )


type Effect
    = FocusOn String
    | None


perform : Effect -> SimulatedEffect Msg
perform effect =
    case effect of
        FocusOn id ->
            SimulatedEffect.Cmd.none

        None ->
            SimulatedEffect.Cmd.none


view : List Modal.Attribute -> Modal.Model -> Html Msg
view modalAttributes model =
    Html.main_ [ Attributes.id "maincontent" ]
        [ Html.button
            [ Attributes.id "open-modal"
            , Events.onClick (OpenModal "open-modal")
            ]
            [ Html.text "Open Modal" ]
        , Modal.view
            { title = modalTitle
            , wrapMsg = ModalMsg
            , content = [ Html.text "Modal Content" ]
            , footer =
                [ Html.button
                    [ Attributes.id lastButtonId
                    ]
                    [ Html.text "Last Button"
                    ]
                ]
            , focus = Focus
            , firstId = Modal.closeButtonId
            , lastId = lastButtonId
            }
            (Modal.closeButton :: modalAttributes)
            model
        ]


lastButtonId : String
lastButtonId =
    "last-button-id"


modalTitle : String
modalTitle =
    "Modal Title"


ensureFocused : String -> ProgramTest a b Effect -> ProgramTest a b Effect
ensureFocused id =
    ensureLastEffect (Expect.equal (FocusOn id))
