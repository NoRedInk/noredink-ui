module Examples.ClickableSvg exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Color exposing (Color)
import Css
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Examples.IconExamples as IconExamples
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.ClickableSvg.V1 as ClickableSvg
import Nri.Ui.Colors.Extra exposing (fromCssColor, toCssColor)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Select.V7 as Select
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Tooltip.V2 as Tooltip
import Nri.Ui.UiIcon.V1 as UiIcon


{-| -}
example : Example State Msg
example =
    { name = "ClickableSvg"
    , version = 1
    , categories = [ Buttons, Icons ]
    , atomicDesignType = Molecule
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \state ->
            let
                ( icon, attributes ) =
                    applySettings state.settings
            in
            [ Html.fromUnstyled (Control.view SetControls state.settings)
            , viewExampleTable icon attributes
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
Tooltip.view
    { trigger =
        \\attrs ->
            ClickableSvg.button "Preview"
                UiIcon.preview
                [ ClickableSvg.width (Css.px 20)
                , ClickableSvg.height (Css.px 20)
                , ClickableSvg.onClick (ShowItWorked "You clicked the preview button!")
                , ClickableSvg.withBorder
                , ClickableSvg.custom attrs
                ]
    , id = "preview-tooltip"
    }
    [ Tooltip.plaintext "Preview"
    , Tooltip.primaryLabel
    , Tooltip.onHover SetPreviewTooltip
    , Tooltip.open state.tooltipPreview
    , Tooltip.smallPadding
    , Tooltip.fitToContent
    , Tooltip.alignEnd (Css.px 28)
    ]
            """
              <|
                Tooltip.view
                    { trigger =
                        \attrs ->
                            ClickableSvg.button "Preview"
                                UiIcon.preview
                                [ ClickableSvg.width (Css.px 20)
                                , ClickableSvg.height (Css.px 20)
                                , ClickableSvg.onClick (ShowItWorked "You clicked the preview button!")
                                , ClickableSvg.withBorder
                                , ClickableSvg.custom attrs
                                ]
                    , id = "preview-tooltip"
                    }
                    [ Tooltip.plaintext "Preview"
                    , Tooltip.primaryLabel
                    , Tooltip.onHover SetPreviewTooltip
                    , Tooltip.open state.tooltipPreview
                    , Tooltip.smallPadding
                    , Tooltip.fitToContent
                    , Tooltip.alignEnd (Css.px 28)
                    ]
            , viewExample
                """
Tooltip.view
    { trigger =
        \\attrs ->
            ClickableSvg.button "Share"
                UiIcon.share
                [ ClickableSvg.width (Css.px 20)
                , ClickableSvg.height (Css.px 20)
                , ClickableSvg.onClick (ShowItWorked "You clicked the share button!")
                , ClickableSvg.custom attrs
                ]
    , id = "share-tooltip"
    }
    [ Tooltip.plaintext "Share"
    , Tooltip.primaryLabel
    , Tooltip.onHover SetShareTooltip
    , Tooltip.open state.tooltipShareTo
    , Tooltip.smallPadding
    , Tooltip.fitToContent
    , Tooltip.onRight
    ]
            """
              <|
                Tooltip.view
                    { trigger =
                        \attrs ->
                            ClickableSvg.button "Share"
                                UiIcon.share
                                [ ClickableSvg.width (Css.px 20)
                                , ClickableSvg.height (Css.px 20)
                                , ClickableSvg.onClick (ShowItWorked "You clicked the share button!")
                                , ClickableSvg.custom attrs
                                ]
                    , id = "share-tooltip"
                    }
                    [ Tooltip.plaintext "Share"
                    , Tooltip.primaryLabel
                    , Tooltip.onHover SetShareTooltip
                    , Tooltip.open state.tooltipShareTo
                    , Tooltip.smallPadding
                    , Tooltip.fitToContent
                    , Tooltip.onRight
                    ]
            ]
    }


viewExampleTable : Svg -> List (ClickableSvg.Attribute Msg) -> Html Msg
viewExampleTable icon attributes =
    let
        viewExampleRow index ( themeName, theme ) =
            Html.tr []
                [ cell index [ Html.text themeName ]
                , cell index [ buttonExample (theme :: attributes) ]
                , cell index [ buttonExample (ClickableSvg.withBorder :: theme :: attributes) ]
                , cell index [ linkExample (theme :: attributes) ]
                , cell index [ linkExample (ClickableSvg.withBorder :: theme :: attributes) ]
                ]

        cell index =
            Html.td
                [ Attributes.css
                    [ if modBy 2 index == 0 then
                        Css.backgroundColor Colors.gray96

                      else
                        Css.backgroundColor Colors.white
                    , Css.padding (Css.px 10)
                    ]
                ]

        buttonExample attributes_ =
            ClickableSvg.button "Button example"
                icon
                (ClickableSvg.onClick (ShowItWorked "You clicked the back button!")
                    :: attributes_
                )

        linkExample attributes_ =
            ClickableSvg.link "Link example"
                icon
                (ClickableSvg.linkSpa "some_link" :: attributes_)
    in
    Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "theme" ]
                , Html.th [ Attributes.colspan 2 ] [ Html.text "button" ]
                , Html.th [ Attributes.colspan 2 ] [ Html.text "link" ]
                ]
            ]
        , Html.tbody [] <|
            List.indexedMap viewExampleRow
                [ ( "primary", ClickableSvg.primary )
                , ( "secondary", ClickableSvg.secondary )
                , ( "danger", ClickableSvg.danger )
                , ( "dangerSecondary", ClickableSvg.dangerSecondary )
                ]
        ]


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
    , settings : Control (Settings Msg)
    }


{-| -}
init : State
init =
    { tooltipPreview = False
    , tooltipShareTo = False
    , settings = initSettings
    }


{-| -}
type Msg
    = ShowItWorked String
    | SetPreviewTooltip Bool
    | SetShareTooltip Bool
    | SetControls (Control (Settings Msg))


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

        SetControls settings ->
            ( { state | settings = settings }, Cmd.none )


type alias Settings msg =
    { icon : Svg
    , disabled : ClickableSvg.Attribute msg
    , width : ClickableSvg.Attribute msg
    , height : ClickableSvg.Attribute msg
    }


applySettings : Control (Settings msg) -> ( Svg, List (ClickableSvg.Attribute msg) )
applySettings settings =
    let
        { icon, disabled, width, height } =
            Control.currentValue settings
    in
    ( icon, [ disabled, width, height ] )


initSettings : Control (Settings msg)
initSettings =
    Control.record Settings
        |> Control.field "icon"
            (Control.choice
                [ ( "arrowLeft", Control.value UiIcon.arrowLeft )
                , ( "unarchive", Control.value UiIcon.unarchive )
                , ( "share", Control.value UiIcon.share )
                , ( "preview", Control.value UiIcon.preview )
                , ( "skip", Control.value UiIcon.skip )
                , ( "copyToClipboard", Control.value UiIcon.copyToClipboard )
                , ( "gift", Control.value UiIcon.gift )
                , ( "home", Control.value UiIcon.home )
                , ( "library", Control.value UiIcon.library )
                , ( "searchInCicle", Control.value UiIcon.searchInCicle )
                ]
            )
        |> Control.field "disabled"
            (Control.map ClickableSvg.disabled (Control.bool False))
        |> Control.field "width"
            (Control.map (Css.px >> ClickableSvg.width) (controlNumber 30))
        |> Control.field "height"
            (Control.map (Css.px >> ClickableSvg.height) (controlNumber 30))


controlNumber : Float -> Control Float
controlNumber default =
    Control.map (String.toFloat >> Maybe.withDefault default)
        (Control.string (String.fromFloat default))
