module Examples.Modal exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Accessibility.Styled.Key as Key
import Browser.Dom as Dom
import Category exposing (Category(..))
import Code
import CommonControls
import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import EventExtras exposing (onClickStopPropagation)
import Example exposing (Example)
import Guidance
import Html.Styled as Html exposing (Html, div, text)
import Html.Styled.Attributes as Attributes exposing (css)
import KeyboardSupport
import Nri.Ui.Button.V11 as Button
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.ClickableText.V5 as ClickableText
import Nri.Ui.Colors.Extra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Modal.V12 as Modal
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V1 as UiIcon
import Task


{-| -}
type alias State =
    { state : Modal.Model
    , settings : Control ViewSettings
    }


{-| -}
init : State
init =
    { state = Modal.init
    , settings = initViewSettings
    }


type alias ViewSettings =
    { title : String
    , titleVisibility : Maybe ( String, Modal.Attribute )
    , theme : Maybe ( String, Modal.Attribute )
    , customCss : Maybe ( String, Modal.Attribute )
    , showX : Bool
    , showFocusOnTitle : Bool
    , showSecondary : Bool
    , dismissOnEscAndOverlayClick : Bool
    , content : String
    , atac : Bool
    }


initViewSettings : Control ViewSettings
initViewSettings =
    Control.record ViewSettings
        |> Control.field "Modal title" (Control.string "Modal Title")
        |> Control.field "Title visibility" (Control.maybe False controlTitleVisibility)
        |> Control.field "Theme" (Control.maybe False controlTheme)
        |> Control.field "Custom css" (Control.maybe False controlCss)
        |> Control.field "X button" (Control.bool True)
        |> Control.field "Focus on title button" (Control.bool True)
        |> Control.field "Close button" (Control.bool True)
        |> Control.field "dismissOnEscAndOverlayClick" (Control.bool True)
        |> Control.field "Content"
            (Control.stringTextarea <|
                String.join "\n\n"
                    [ "Muffin liquorice powder liquorice jujubes biscuit cookie candy canes lemon drops. Liquorice powder carrot cake dragée icing tootsie roll apple pie lemon drops lemon drops. Jujubes danish bear claw cotton candy. Dragée apple pie tiramisu. Sugar plum dessert pastry marzipan chocolate cake dragée sesame snaps. Marshmallow gingerbread lemon drops. Brownie chocolate fruitcake pastry. Powder jelly beans jujubes. Croissant macaroon dessert cookie candy canes jelly jujubes. Muffin liquorice ice cream wafer donut danish soufflé dragée chocolate bar. Candy croissant candy wafer toffee lemon drops croissant danish sugar plum. Cookie cake candy canes. Pastry powder muffin soufflé tootsie roll sweet cookie tiramisu."
                    , "Candy cake danish gingerbread. Caramels toffee cupcake toffee sweet. Gummi bears candy cheesecake sweet. Pie gingerbread sugar plum halvah muffin icing marzipan wafer icing. Candy fruitcake gummies icing marzipan. Halvah jelly beans candy candy canes biscuit bonbon sesame snaps. Biscuit carrot cake croissant cake chocolate lollipop candy biscuit croissant. Topping jujubes apple pie croissant chocolate cake. Liquorice cookie dragée gummies cotton candy fruitcake lemon drops candy canes. Apple pie lemon drops gummies cake chocolate bar cake jelly-o tiramisu. Chocolate bar icing pudding marshmallow cake soufflé soufflé muffin. Powder lemon drops biscuit sugar plum cupcake carrot cake powder cake dragée. Bear claw gummi bears liquorice sweet roll."
                    ]
            )
        |> Control.field "Show ATAC" (Control.bool False)


controlTitleVisibility : Control ( String, Modal.Attribute )
controlTitleVisibility =
    CommonControls.choice moduleName
        [ ( "hideTitle", Modal.hideTitle )
        , ( "showTitle", Modal.showTitle )
        ]


controlTheme : Control ( String, Modal.Attribute )
controlTheme =
    CommonControls.choice moduleName
        [ ( "warning", Modal.warning )
        , ( "info", Modal.info )
        ]


controlCss : Control ( String, Modal.Attribute )
controlCss =
    CommonControls.choice moduleName
        [ ( "css [ Css.borderRadius Css.zero ]", Modal.css [ Css.borderRadius Css.zero ] )
        , ( "css [ Css.width (Css.px 900) ]", Modal.css [ Css.width (Css.px 900) ] )
        ]


moduleName : String
moduleName =
    "Modal"


version : Int
version =
    12


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Layout, Messaging ]
    , keyboardSupport =
        [ { keys = [ KeyboardSupport.Tab ]
          , result = "Moves focus to the next button within the modal or wraps back to the first element within the modal."
          }
        , { keys = [ KeyboardSupport.Tab, KeyboardSupport.Shift ]
          , result = "Moves focus to the previous button within the modal or wraps back to the last element within the modal."
          }
        , { keys = [ KeyboardSupport.Esc ]
          , result = "If 'dismissOnEscAndOverlayClick' is set to true, closes the modal. Else, does nothing."
          }
        ]
    , init = init
    , update = update
    , subscriptions = subscriptions
    , preview =
        [ -- faking a mini version of the Modal component to give Component Catalog users a sense of what the
          -- component might look like
          div
            [ css
                [ Css.backgroundColor (Nri.Ui.Colors.Extra.withAlpha 0.9 Colors.navy)
                , Css.borderRadius (Css.px 4)
                , Css.padding2 (Css.px 25) Css.zero
                , Css.displayFlex
                , Css.alignItems Css.center
                , Css.justifyContent Css.center
                ]
            ]
            [ div
                [ css
                    [ Css.backgroundColor Colors.white
                    , Css.padding (Css.px 10)
                    , Css.borderRadius (Css.px 10)
                    , Css.boxShadow5 Css.zero (Css.px 1) (Css.px 10) Css.zero (Css.rgba 0 0 0 0.35)
                    , Css.textAlign Css.center
                    , Css.color Colors.navy
                    , Fonts.baseFont
                    , Css.margin Css.auto
                    , Css.width (Css.px 100)
                    , Css.height (Css.px 60)
                    , Css.fontSize (Css.px 10)
                    , Css.fontWeight Css.bold
                    , Css.position Css.relative
                    ]
                ]
                [ text "Modal"
                , ClickableSvg.link "Close"
                    UiIcon.x
                    [ ClickableSvg.exactWidth 10
                    , ClickableSvg.exactHeight 10
                    , ClickableSvg.css
                        [ Css.position absolute
                        , Css.top (Css.px 10)
                        , Css.right (Css.px 10)
                        ]
                    , ClickableSvg.custom [ Key.tabbable False ]
                    ]
                ]
            ]
        ]
    , about =
        [ Guidance.useATACGuide moduleName
        , Text.smallBody
            [ Text.markdown "Use `Button.modal` and/or `ClickableText.modal` within the `Modal` footer." ]
        ]
    , view =
        \ellieLinkConfig state ->
            let
                settings =
                    Control.currentValue state.settings
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateSettings
                , settings = state.settings
                , mainType = Just "RootHtml.Html Msg"
                , extraCode = [ "\n\ntype Msg = ModalMsg Modal.Msg | Focus String" ]
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        let
                            code =
                                [ Code.fromModule moduleName "view"
                                    ++ Code.record
                                        [ ( "title", Code.string settings.title )
                                        , ( "wrapMsg", "ModalMsg" )
                                        , ( "content", "[] -- The body of the modal goes in here" )
                                        , ( "footer", "[] -- Use Button.modal and ClickableText.modal for standard styles" )
                                        , ( "focus", "Focus" )
                                        , ( "firstId"
                                          , Code.string
                                                (if settings.showX then
                                                    Modal.closeButtonId

                                                 else if settings.showFocusOnTitle then
                                                    continueButtonId

                                                 else
                                                    closeClickableTextId
                                                )
                                          )
                                        , ( "lastId"
                                          , Code.string
                                                (if settings.showSecondary then
                                                    closeClickableTextId

                                                 else if settings.showFocusOnTitle then
                                                    continueButtonId

                                                 else
                                                    Modal.closeButtonId
                                                )
                                          )
                                        ]
                                , Code.listMultiline
                                    ([ if settings.showX then
                                        Just "Modal.closeButton"

                                       else
                                        Nothing
                                     , Maybe.map Tuple.first settings.titleVisibility
                                     , Maybe.map Tuple.first settings.theme
                                     , Maybe.map Tuple.first settings.customCss
                                     , if settings.atac then
                                        (Code.fromModule moduleName "atac"
                                            ++ Code.withParens
                                                ("text "
                                                    ++ Code.string "Fake ATAC -- use the real one in real code!"
                                                )
                                        )
                                            |> Just

                                       else
                                        Nothing
                                     ]
                                        |> List.filterMap identity
                                    )
                                    1
                                , Code.newlineWithIndent 1
                                , "modalState"
                                ]
                                    |> String.join ""
                        in
                        [ { sectionName = "Example", code = code } ]
                }
            , Heading.h2
                [ Heading.plaintext "Customizable Example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , launchModalButton settings
            , Modal.view
                { title = settings.title
                , wrapMsg = ModalMsg
                , content = [ viewModalContent settings.content ]
                , footer =
                    List.filterMap identity
                        [ if settings.showFocusOnTitle then
                            Just focusOnModalTitle

                          else
                            Nothing
                        , if settings.showSecondary then
                            Just closeClickableText

                          else
                            Nothing
                        ]
                , focus = Focus
                , firstId =
                    if settings.showX then
                        Modal.closeButtonId

                    else if settings.showFocusOnTitle then
                        continueButtonId

                    else
                        closeClickableTextId
                , lastId =
                    if settings.showSecondary then
                        closeClickableTextId

                    else if settings.showFocusOnTitle then
                        continueButtonId

                    else
                        Modal.closeButtonId
                }
                ([ if settings.showX then
                    Just Modal.closeButton

                   else
                    Nothing
                 , Maybe.map Tuple.second settings.titleVisibility
                 , Maybe.map Tuple.second settings.theme
                 , Maybe.map Tuple.second settings.customCss
                 , if settings.atac then
                    Just (Modal.atac fakeATAC)

                   else
                    Nothing
                 ]
                    |> List.filterMap identity
                )
                state.state
                |> List.singleton
                |> div [ onClickStopPropagation SwallowEvent ]
            ]
    }


fakeATAC : Html msg
fakeATAC =
    Html.aside
        [ Attributes.css
            [ Css.position Css.fixed
            , Css.bottom Css.zero
            , Css.left Css.zero
            , Css.maxWidth (Css.px 480)
            , Css.backgroundColor Colors.gray96
            , Css.borderTop3 (Css.px 1) Css.solid Colors.gray75
            , Css.borderRight3 (Css.px 1) Css.solid Colors.gray75
            , Css.minHeight (Css.px 80)
            , Css.padding (Css.px 8)
            , Fonts.baseFont
            , Css.zIndex (Css.int 1)
            ]
        ]
        [ Heading.h2
            [ Heading.plaintext "(Fake) Announcement history"
            ]
        , Text.smallBody
            [ Text.html
                [ text "NRI employees can learn more about the real ATAC in "
                , ClickableText.link "Assistive Technology Announcement Center (“ATAC”)"
                    [ ClickableText.appearsInline
                    , ClickableText.linkExternal "https://paper.dropbox.com/doc/Assistive-Technology-Announcement-Center-ATAC--B_GuqwWltzU432ueq7p6Z42mAg-bOnmcnzOj631NRls1IBe3"
                    ]
                ]
            ]
        ]


launchModalButton : ViewSettings -> Html Msg
launchModalButton settings =
    let
        launchId =
            "launch-modal"

        startFocusId =
            if settings.showFocusOnTitle then
                Just continueButtonId

            else if settings.showX then
                Just Modal.closeButtonId

            else if settings.showSecondary then
                Just closeClickableTextId

            else
                Nothing
    in
    Button.button "Launch Modal"
        [ case startFocusId of
            Just autofocusElementId ->
                Button.onClick
                    (OpenModal
                        { startFocusOn = autofocusElementId
                        , returnFocusTo = launchId
                        }
                    )

            Nothing ->
                Button.disabled
        , Button.custom [ Attributes.id launchId ]
        , Button.secondary
        ]


viewModalContent : String -> Html msg
viewModalContent content =
    Text.mediumBody
        [ Text.css [ whiteSpace preLine ]
        , Text.plaintext content
        ]


continueButtonId : String
continueButtonId =
    "continue-button-id"


focusOnModalTitle : Html Msg
focusOnModalTitle =
    Button.button "Focus on modal title"
        [ Button.onClick (Focus Modal.titleId)
        , Button.id continueButtonId
        , Button.modal
        ]


closeClickableTextId : String
closeClickableTextId =
    "close-clickable-text-id"


closeClickableText : Html Msg
closeClickableText =
    ClickableText.button "Close"
        [ ClickableText.onClick CloseModal
        , ClickableText.modal
        , ClickableText.id closeClickableTextId
        ]


{-| -}
type Msg
    = OpenModal { startFocusOn : String, returnFocusTo : String }
    | ModalMsg Modal.Msg
    | CloseModal
    | UpdateSettings (Control ViewSettings)
    | Focus String
    | Focused (Result Dom.Error ())
    | SwallowEvent


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    let
        settings =
            state.settings

        updateConfig =
            { dismissOnEscAndOverlayClick =
                (Control.currentValue settings).dismissOnEscAndOverlayClick
            }
    in
    case msg of
        OpenModal config ->
            let
                ( newState, startFocusOn ) =
                    Modal.open config
            in
            ( { state | state = newState }
            , Task.attempt Focused (Dom.focus startFocusOn)
            )

        ModalMsg modalMsg ->
            case Modal.update updateConfig modalMsg state.state of
                ( newState, maybeFocus ) ->
                    ( { state | state = newState }
                    , Maybe.map (Task.attempt Focused << Dom.focus) maybeFocus
                        |> Maybe.withDefault Cmd.none
                    )

        CloseModal ->
            let
                ( newState, maybeFocus ) =
                    Modal.close state.state
            in
            ( { state | state = newState }
            , Maybe.map (Task.attempt Focused << Dom.focus) maybeFocus
                |> Maybe.withDefault Cmd.none
            )

        UpdateSettings value ->
            ( { state | settings = value }, Cmd.none )

        Focus id ->
            ( state, Task.attempt Focused (Dom.focus id) )

        Focused _ ->
            ( state, Cmd.none )

        SwallowEvent ->
            ( state, Cmd.none )


{-| -}
subscriptions : State -> Sub Msg
subscriptions model =
    Sub.map ModalMsg (Modal.subscriptions model.state)
