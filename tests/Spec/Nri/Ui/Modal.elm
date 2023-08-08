module Spec.Nri.Ui.Modal exposing (spec)

import Accessibility.Key as Key
import Browser.Dom as Dom
import Expect
import Html.Attributes
import Html.Styled as Html exposing (Html, toUnstyled)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Encode as Encode
import Nri.Ui.Modal.V11 as Modal
import ProgramTest exposing (..)
import SimulatedEffect.Cmd
import Spec.KeyboardHelpers exposing (pressTabBackKey, pressTabKey)
import Task
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


spec : Test
spec =
    describe "Nri.Ui.Modal"
        [ test "titleId is attached to the modal title" <|
            \() ->
                start
                    |> clickButton "Open Modal"
                    |> ensureViewHas
                        [ id Modal.titleId
                        , tag "h1"
                        , containing [ text modalTitle ]
                        , attribute (Key.tabbable False)
                        ]
                    |> done
        , test "focus wraps from the modal title correctly" <|
            \() ->
                start
                    |> clickButton "Open Modal"
                    |> tabBackWithinModal Modal.titleId
                    |> ensureLastEffect (Expect.equal (FocusOn lastButtonId))
                    |> done
        , test "focus wraps from the close button correctly" <|
            \() ->
                start
                    |> clickButton "Open Modal"
                    |> tabBackWithinModal Modal.closeButtonId
                    |> ensureLastEffect (Expect.equal (FocusOn lastButtonId))
                    |> done
        , test "focus wraps from the last button correctly" <|
            \() ->
                start
                    |> clickButton "Open Modal"
                    |> tabForwardWithinModal lastButtonId
                    |> ensureLastEffect (Expect.equal (FocusOn Modal.closeButtonId))
                    |> done
        ]


tabBackWithinModal : String -> ProgramTest a b c -> ProgramTest a b c
tabBackWithinModal onElementId =
    pressTabBackKey { targetDetails = [ ( "id", Encode.string onElementId ) ] } focusTrapNode


tabForwardWithinModal : String -> ProgramTest a b c -> ProgramTest a b c
tabForwardWithinModal onElementId =
    pressTabKey { targetDetails = [ ( "id", Encode.string onElementId ) ] } focusTrapNode


focusTrapNode : List Selector
focusTrapNode =
    [ attribute (Html.Attributes.attribute "data-testid" "focus-trap-node") ]


start : ProgramTest Modal.Model Msg Effect
start =
    createElement
        { init = \_ -> ( Modal.init, None )
        , view = toUnstyled << view
        , update = update
        }
        |> withSimulatedEffects perform
        |> ProgramTest.start ()


type Msg
    = OpenModal String
    | ModalMsg Modal.Msg
    | Focus String
    | Focused (Result Dom.Error ())


update : Msg -> Modal.Model -> ( Modal.Model, Effect )
update msg model =
    case msg of
        OpenModal returnFocusTo ->
            let
                ( newModel, cmd ) =
                    Modal.open
                        { startFocusOn = Modal.closeButtonId
                        , returnFocusTo = returnFocusTo
                        }
            in
            ( newModel, ModalEffect cmd )

        ModalMsg modalMsg ->
            let
                ( newModel, cmd ) =
                    Modal.update
                        { dismissOnEscAndOverlayClick = True }
                        modalMsg
                        model
            in
            ( newModel, ModalEffect cmd )

        Focus id ->
            ( model, FocusOn id )

        Focused _ ->
            ( model, None )


type Effect
    = ModalEffect (Cmd Modal.Msg)
    | FocusOn String
    | None


perform : Effect -> SimulatedEffect Msg
perform effect =
    case effect of
        ModalEffect modalMsg ->
            SimulatedEffect.Cmd.none

        FocusOn id ->
            SimulatedEffect.Cmd.none

        None ->
            SimulatedEffect.Cmd.none


view : Modal.Model -> Html Msg
view model =
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
            , focusTrap =
                { focus = Focus
                , firstId = Modal.closeButtonId
                , lastId = lastButtonId
                }
            }
            [ Modal.closeButton
            ]
            model
        ]


lastButtonId : String
lastButtonId =
    "last-button-id"


modalTitle : String
modalTitle =
    "Modal Title"
