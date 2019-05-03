module Examples.Modal exposing (Msg, State, example, init, update)

{-|

@docs Msg, State, example, init, update

-}

import Accessibility.Styled as Html exposing (Html, div, h3, p, text)
import Assets
import Css exposing (..)
import Html.Styled.Attributes exposing (css)
import ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Button.V5 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Modal.V3 as Modal


{-| -}
type Msg
    = DismissModal
    | ShowModal ModalType


{-| -}
type alias State =
    { modal : Maybe ModalType }


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.Modal.V3"
    , category = Modals
    , content =
        [ case state.modal of
            Just modal ->
                viewModal modal

            Nothing ->
                text ""
        , viewButtons
        ]
            |> List.map (Html.map parentMessage)
    }


{-| -}
init : State
init =
    { modal = Nothing }


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        DismissModal ->
            ( { state | modal = Nothing }, Cmd.none )

        ShowModal modalType ->
            ( { state | modal = Just modalType }, Cmd.none )



-- INTERNAL


type ModalType
    = InfoModal
    | WarningModal
    | NoButtonModal
    | NoDismissModal
    | NoHeading
    | ScrolledContentModal


viewButtons : Html Msg
viewButtons =
    [ ( "Info Modal", "Modal.info", InfoModal )
    , ( "Warning Modal", "Modal.warning", WarningModal )
    , ( "No Button Modal", "Modal.info { ... footerContent = [] ... }", NoButtonModal )
    , ( "No Dismiss Modal", "Modal.info { ... onDismiss Nothing ... }", NoDismissModal )
    , ( "No Heading", "Modal.info { ... visibleTitle = False ... }", NoHeading )
    , ( "Scrolled Content"
      , "Modal.info { content = Html.text 'so much stuff' }"
      , ScrolledContentModal
      )
    ]
        |> List.map modalLaunchButton
        |> div []


modalLaunchButton : ( String, String, ModalType ) -> Html Msg
modalLaunchButton ( label, details, modalType ) =
    div []
        [ h3 [] [ text label ]
        , p [] [ text details ]
        , Button.button
            { onClick = ShowModal modalType
            , size = Button.Small
            , style = Button.Secondary
            , width = Button.WidthUnbounded
            }
            { label = label
            , state = Button.Enabled
            , icon = Nothing
            }
        ]


viewModal : ModalType -> Html Msg
viewModal modal =
    case modal of
        InfoModal ->
            Modal.info Assets.assets
                { title = "Info Modal"
                , visibleTitle = True
                , content = text "This is where the content goes!"
                , onDismiss = Just DismissModal
                , width = Nothing
                , footerContent =
                    [ modalFooterButton "Primary" Button.Primary
                    , modalFooterButton "Cancel" Button.Borderless
                    ]
                }

        WarningModal ->
            Modal.warning Assets.assets
                { title = "Warning Modal"
                , visibleTitle = True
                , content = text "This is where the content goes!"
                , onDismiss = Just DismissModal
                , width = Nothing
                , footerContent =
                    [ modalFooterButton "Primary" Button.Danger
                    , modalFooterButton "Cancel" Button.Borderless
                    ]
                }

        NoButtonModal ->
            Modal.info Assets.assets
                { title = "No Buttons"
                , visibleTitle = True
                , content = text "This is where the content goes!"
                , onDismiss = Just DismissModal
                , width = Nothing
                , footerContent = []
                }

        NoDismissModal ->
            Modal.info Assets.assets
                { title = "No Dismiss"
                , visibleTitle = True
                , content = text "This is where the content goes!"
                , onDismiss = Nothing
                , width = Nothing
                , footerContent =
                    [ modalFooterButton "Primary" Button.Primary
                    , modalFooterButton "Cancel" Button.Borderless
                    ]
                }

        NoHeading ->
            Modal.info Assets.assets
                { title = "Hidden title"
                , onDismiss = Just DismissModal
                , visibleTitle = False
                , footerContent = []
                , width = Nothing
                , content =
                    div
                        [ css
                            [ width (pct 100)
                            , height (px 200)
                            , backgroundColor Colors.gray75
                            , border3 (px 1) dashed Colors.gray20
                            ]
                        ]
                        [ text "Imagine an image" ]
                }

        ScrolledContentModal ->
            Modal.info Assets.assets
                { title = "Scrolled Content"
                , onDismiss = Just DismissModal
                , visibleTitle = True
                , footerContent = [ modalFooterButton "Primary" Button.Primary ]
                , width = Nothing
                , content =
                    div []
                        [ text "\nIt was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going direct the other way – in short, the period was so far like the present period, that some of its noisiest authorities insisted on its being received, for good or for evil, in the superlative degree of comparison only.\n\n\nIt was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going direct the other way – in short, the period was so far like the present period, that some of its noisiest authorities insisted on its being received, for good or for evil, in the superlative degree of comparison only.\n\nIt was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going direct the other way – in short, the period was so far like the present period, that some of its noisiest authorities insisted on its being received, for good or for evil, in the superlative degree of comparison only.\n                          "
                        , div
                            [ css
                                [ width (pct 100)
                                , height (px 200)
                                , backgroundColor Colors.gray75
                                , border3 (px 1) dashed Colors.gray20
                                ]
                            ]
                            [ text "Imagine an image" ]
                        ]
                }


modalFooterButton : String -> Button.ButtonStyle -> Html Msg
modalFooterButton label style =
    Button.button
        { onClick = DismissModal
        , size = Button.Large
        , style = style
        , width = Button.WidthExact 230
        }
        { label = label
        , state = Button.Enabled
        , icon = Nothing
        }
