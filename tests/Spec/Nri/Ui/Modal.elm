module Spec.Nri.Ui.Modal exposing (spec)

import Browser.Dom as Dom
import Html.Styled as Html exposing (Html, toUnstyled)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui.Modal.V11 as Modal
import ProgramTest exposing (..)
import Task
import Test exposing (..)
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
                        ]
                    |> done
        ]


start : ProgramTest Modal.Model Msg (Cmd Msg)
start =
    ProgramTest.createElement
        { init = \_ -> init
        , view = toUnstyled << view
        , update = update
        }
        |> ProgramTest.start ()


init : ( Modal.Model, Cmd Msg )
init =
    let
        ( model, cmd ) =
            -- When we load the page with a modal already open, we should return
            -- the focus someplace sensible when the modal closes.
            -- [This article](https://developer.paciellogroup.com/blog/2018/06/the-current-state-of-modal-dialog-accessibility/) recommends
            -- focusing the main or body.
            Modal.open
                { startFocusOn = Modal.closeButtonId
                , returnFocusTo = "maincontent"
                }
    in
    ( model, Cmd.map ModalMsg cmd )


type Msg
    = OpenModal String
    | ModalMsg Modal.Msg
    | CloseModal
    | Focus String
    | Focused (Result Dom.Error ())


update : Msg -> Modal.Model -> ( Modal.Model, Cmd Msg )
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
            ( newModel, Cmd.map ModalMsg cmd )

        ModalMsg modalMsg ->
            let
                ( newModel, cmd ) =
                    Modal.update
                        { dismissOnEscAndOverlayClick = True }
                        modalMsg
                        model
            in
            ( newModel, Cmd.map ModalMsg cmd )

        CloseModal ->
            let
                ( newModel, cmd ) =
                    Modal.close model
            in
            ( newModel, Cmd.map ModalMsg cmd )

        Focus id ->
            ( model, Task.attempt Focused (Dom.focus id) )

        Focused _ ->
            ( model, Cmd.none )


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
            , footer = []
            , focusTrap =
                { focus = Focus
                , firstId = Modal.closeButtonId
                , lastId = Modal.closeButtonId
                }
            }
            [ Modal.closeButton
            ]
            model
        ]


modalTitle : String
modalTitle =
    "Modal Title"
