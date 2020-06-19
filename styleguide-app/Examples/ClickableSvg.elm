module Examples.ClickableSvg exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Color exposing (Color)
import Css
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Html.Styled as Html
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Nri.Ui.ClickableSvg.V1 as ClickableSvg
import Nri.Ui.Colors.Extra exposing (fromCssColor, toCssColor)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Select.V7 as Select
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.ClickableSvg.V1"
    , categories = [ Buttons, Icons ]
    , atomicDesignType = AtomicDesignType.Molecule
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \state ->
            [ viewExample "ClickableSvg.button \"Back\" UiIcon.arrowLeft [ ClickableSvg.onClick OnClickMsg ]" <|
                ClickableSvg.button "Back"
                    UiIcon.arrowLeft
                    [ ClickableSvg.onClick (ShowItWorked "You clicked the back button!") ]
            , viewExample "ClickableSvg.link \"Back\" UiIcon.arrowLeft [ ClickableSvg.linkSpa \"some_link\" ]" <|
                ClickableSvg.link "Back" UiIcon.arrowLeft [ ClickableSvg.linkSpa "some_link" ]
            , viewExample "ClickableSvg.button \"Disabled\" UiIcon.arrowLeft [ ClickableSvg.disabled True ]" <|
                ClickableSvg.button "Disabled" UiIcon.arrowLeft [ ClickableSvg.disabled True ]
            , viewExample "ClickableSvg.link \"Disabled\" UiIcon.arrowLeft [ ClickableSvg.disabled True ]" <|
                ClickableSvg.link "Disabled" UiIcon.arrowLeft [ ClickableSvg.disabled True ]
            , viewExample
                """
ClickableSvg.button "Go to tutorial"
    UiIcon.footsteps
    [ ClickableSvg.width (Css.px 30)
    , ClickableSvg.height (Css.px 30)
    , ClickableSvg.onClick (ShowItWorked "You clicked the tutorials button!")
    , ClickableSvg.custom [ Attributes.id "clickable-svg-customized-example-id" ]
    , ClickableSvg.css [ Css.border3 (Css.px 1) Css.dashed Colors.azure ]
    ]
                """
              <|
                ClickableSvg.button "Go to tutorial"
                    UiIcon.footsteps
                    [ ClickableSvg.width (Css.px 30)
                    , ClickableSvg.height (Css.px 30)
                    , ClickableSvg.onClick (ShowItWorked "You clicked the tutorials button!")
                    , ClickableSvg.custom [ Attributes.id "clickable-svg-customized-example-id" ]
                    , ClickableSvg.css [ Css.border3 (Css.px 1) Css.dashed Colors.azure ]
                    ]
            , viewExample
                """
ClickableSvg.button "Preview"
    UiIcon.preview
    [ ClickableSvg.width (Css.px 20)
    , ClickableSvg.height (Css.px 20)
    , ClickableSvg.onClick (ShowItWorked "You clicked the preview button!")
    , ClickableSvg.withTooltipAbove { id = "preview", isOpen = state.tooltipPreview, onShow = SetPreviewTooltip }
    ]
            """
              <|
                ClickableSvg.button "Preview"
                    UiIcon.preview
                    [ ClickableSvg.width (Css.px 20)
                    , ClickableSvg.height (Css.px 20)
                    , ClickableSvg.onClick (ShowItWorked "You clicked the preview button!")
                    , ClickableSvg.withTooltipAbove
                        { id = "preview"
                        , isOpen = state.tooltipPreview
                        , onShow = SetPreviewTooltip
                        }
                    ]
            , viewExample
                """
ClickableSvg.button "Share"
    UiIcon.share
    [ ClickableSvg.width (Css.px 20)
    , ClickableSvg.height (Css.px 20)
    , ClickableSvg.onClick (ShowItWorked "You clicked the Share button!")
    , ClickableSvg.withTooltipBelow { id = "share", isOpen = state.tooltipShareTo, onShow = SetShareTooltip }
    ]
            """
              <|
                ClickableSvg.button "Share"
                    UiIcon.share
                    [ ClickableSvg.width (Css.px 20)
                    , ClickableSvg.height (Css.px 20)
                    , ClickableSvg.onClick (ShowItWorked "You clicked the share button!")
                    , ClickableSvg.withTooltipBelow
                        { id = "share"
                        , isOpen = state.tooltipShareTo
                        , onShow = SetShareTooltip
                        }
                    ]
            ]
    }


viewExample : String -> Html.Html msg -> Html.Html msg
viewExample code html =
    Html.div
        [ Attributes.css [ Css.displayFlex, Css.alignItems Css.center ] ]
        [ html
        , viewCode code
        ]


viewCode : String -> Html.Html msg
viewCode renderStrategy =
    Html.code
        [ Attributes.css
            [ Css.width (Css.px 400)
            , Css.marginLeft (Css.px 20)
            ]
        ]
        [ Html.pre [] [ Html.text renderStrategy ] ]


{-| -}
type alias State =
    { tooltipPreview : Bool
    , tooltipShareTo : Bool
    }


{-| -}
init : State
init =
    { tooltipPreview = False
    , tooltipShareTo = False
    }


{-| -}
type Msg
    = ShowItWorked String
    | SetPreviewTooltip Bool
    | SetShareTooltip Bool


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ShowItWorked message ->
            let
                _ =
                    Debug.log "ClickableSvg" message
            in
            ( state, Cmd.none )

        SetPreviewTooltip bool ->
            ( { state | tooltipPreview = bool }, Cmd.none )

        SetShareTooltip bool ->
            ( { state | tooltipShareTo = bool }, Cmd.none )
