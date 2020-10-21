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
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
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
    , version = 2
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
Tooltip.view
    { trigger =
        \\attrs ->
            ClickableSvg.button "Preview"
                UiIcon.preview
                [ ClickableSvg.onClick (ShowItWorked "You clicked the preview button!")
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
    ]
            """
              <|
                Tooltip.view
                    { trigger =
                        \attrs ->
                            ClickableSvg.button "Preview"
                                UiIcon.preview
                                [ ClickableSvg.onClick (ShowItWorked "You clicked the preview button!")
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
                , cell index [ linkExample (theme :: attributes) ]
                , cell index [ buttonExample (ClickableSvg.withBorder :: theme :: attributes) ]
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
                , Html.th [ Attributes.colspan 2 ] [ Html.text "" ]
                , Html.th [ Attributes.colspan 2 ] [ Html.text "withBorder" ]
                ]
            ]
        , Html.tbody [] <|
            List.indexedMap viewExampleRow
                [ ( "primary", ClickableSvg.primary )
                , ( "secondary", ClickableSvg.secondary )
                , ( "danger", ClickableSvg.danger )
                , ( "dangerSecondary", ClickableSvg.dangerSecondary )
                ]
        , Html.tfoot []
            [ Html.tr []
                [ Html.td [] [ Html.text "" ]
                , Html.td [] [ Html.text "button" ]
                , Html.td [] [ Html.text "link" ]
                , Html.td [] [ Html.text "button" ]
                , Html.td [] [ Html.text "link" ]
                ]
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
    , size : ClickableSvg.Attribute msg
    , width : Maybe (ClickableSvg.Attribute msg)
    }


applySettings : Control (Settings msg) -> ( Svg, List (ClickableSvg.Attribute msg) )
applySettings settings =
    let
        { icon, disabled, size, width } =
            Control.currentValue settings
    in
    ( icon, List.filterMap identity [ Just disabled, Just size, width ] )


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
        |> Control.field "size"
            (Control.choice
                [ ( "small", Control.value ClickableSvg.small )
                , ( "medium", Control.value ClickableSvg.medium )
                , ( "large", Control.value ClickableSvg.large )
                ]
            )
        |> Control.field "exactWidth"
            (Control.maybe False (Control.map ClickableSvg.exactWidth (controlInt 40)))


controlInt : Int -> Control Int
controlInt default =
    Control.map (String.toInt >> Maybe.withDefault default)
        (Control.string (String.fromInt default))
