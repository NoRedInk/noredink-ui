module Examples.IconExamples exposing
    ( example
    , Settings, Msg
    , Group
    , preview, previewCustomSize
    )

{-|

@docs example
@docs Settings, Msg
@docs Group
@docs preview, previewCustomSize

-}

import Category exposing (Category(..))
import Code
import Css
import Css.Global
import Example exposing (Example)
import ExampleSection
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Nri.Ui.Checkbox.V7 as Checkbox
import Nri.Ui.Colors.Extra exposing (fromCssColor, toCssColor)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Select.V9 as Select
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Text.V6 as Text
import Nri.Ui.TextInput.V7 as TextInput
import SolidColor exposing (SolidColor)


type alias Config =
    { moduleName : String
    , version : Int
    , label : String
    , name : String
    , icon : Svg
    , renderSvgCode : String -> String
    , preview : List (Html Never)
    , all : List Group
    }


example : Config -> Example Settings Msg
example config =
    { name = config.moduleName
    , version = config.version
    , categories = [ Icons ]
    , keyboardSupport = []
    , state = init config
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = config.preview
    , about = []
    , view = \ellieLinkConfig settings -> view settings config.all
    }


{-| -}
preview : List Svg.Svg -> List (Html msg)
preview =
    previewCustomSize ( Just 30, Just 30 )


{-| -}
previewCustomSize : ( Maybe Float, Maybe Float ) -> List Svg.Svg -> List (Html msg)
previewCustomSize ( width, height ) icons =
    let
        setWidth =
            Maybe.map (Svg.withWidth << Css.px) width
                |> Maybe.withDefault identity

        setHeight =
            Maybe.map (Svg.withHeight << Css.px) height
                |> Maybe.withDefault identity
    in
    [ Html.div
        [ css
            [ Css.displayFlex
            , Css.flexWrap Css.wrap
            , Css.property "gap" "10px"
            , Css.color Colors.gray45
            ]
        ]
        (List.map
            (setWidth >> setHeight >> Svg.toHtml)
            icons
        )
    ]


{-| -}
type alias Settings =
    { showIconName : Bool
    , iconSelectorExpanded : Bool
    , color : SolidColor
    , width : Float
    , height : Float
    , icon : ( String, Svg )
    , label : String
    , showBorder : Bool
    , renderSvgCode : String -> String
    }


{-| -}
init : Config -> Settings
init { label, name, icon, renderSvgCode } =
    { showIconName = False
    , iconSelectorExpanded = False
    , color = fromCssColor Colors.greenDark
    , width = 100
    , height = 100
    , icon = ( name, icon )
    , label = label
    , showBorder = True
    , renderSvgCode = renderSvgCode
    }


{-| -}
type Msg
    = ShowNames Bool
    | SetIcon ( String, Svg )
    | SetColor (Result String SolidColor)
    | SetWidth (Maybe Float)
    | SetHeight (Maybe Float)
    | SetLabel String
    | SetBorder Bool


{-| -}
update : Msg -> Settings -> ( Settings, Cmd msg )
update msg state =
    case msg of
        ShowNames showIconName ->
            ( { state | showIconName = showIconName }
            , Cmd.none
            )

        SetIcon svg ->
            ( { state | icon = svg }
            , Cmd.none
            )

        SetColor (Ok color) ->
            ( { state | color = color }
            , Cmd.none
            )

        SetColor (Err err) ->
            ( state, Cmd.none )

        SetWidth (Just width) ->
            ( { state | width = width }, Cmd.none )

        SetWidth Nothing ->
            ( state, Cmd.none )

        SetHeight (Just height) ->
            ( { state | height = height }, Cmd.none )

        SetHeight Nothing ->
            ( state, Cmd.none )

        SetLabel label ->
            ( { state | label = label }, Cmd.none )

        SetBorder showBorder ->
            ( { state | showBorder = showBorder }, Cmd.none )


{-| -}
viewSettings : Settings -> Html Msg
viewSettings { showIconName } =
    Checkbox.view
        { label = "Show names"
        , selected = Checkbox.selectedFromBool showIconName
        }
        [ Checkbox.id "show-icon-name-checkbox"
        , Checkbox.onCheck ShowNames
        ]


type alias Group =
    ( String
    , List ( String, Svg.Svg, List Css.Style )
    )


{-| -}
view : Settings -> List Group -> List (Html Msg)
view settings groups =
    let
        viewExampleSection ( group, values ) =
            viewWithCustomStyles settings group values
    in
    ExampleSection.sectionWithCss "About" [ Css.flex (Css.int 1) ] Text.smallBody [ aboutMsg ]
        :: Heading.h2 [ Heading.plaintext "Grouped Icons", Heading.css [ Css.marginTop (Css.px 10) ] ]
        :: viewSettings settings
        :: List.map viewExampleSection groups
        ++ [ Html.section []
                [ Heading.h2
                    [ Heading.plaintext "Example Usage"
                    , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                    ]
                , viewSingularExampleSettings groups settings
                , viewResults settings
                ]
           ]


aboutMsg : Text.Attribute msg
aboutMsg =
    Text.markdown
        """
Our icons are Elm SVGs, not separate files or sprites. We use an opaque type to represent them, which enables nice type-safe composability across our components.

We use Pattern #5: `<svg>` + `role='img'` + `<title>` from [Accessible SVGs: Perfect Patterns For Screen Reader Users](https://www.smashingmagazine.com/2021/05/accessible-svg-patterns-comparison/) for non-decorative images.

When the svg is decorative (which is the default), we add `aria-hidden=true` to the svg node.
"""


viewWithCustomStyles : Settings -> String -> List ( String, Svg.Svg, List Css.Style ) -> Html msg
viewWithCustomStyles { showIconName } headerText icons =
    Html.section
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.property "gap" "10px"
            , Css.marginTop (Css.px 10)
            ]
        ]
        [ Heading.h2
            [ Heading.css
                [ Css.width (Css.px 150)
                , Css.fontSize (Css.px 16)
                , Css.lineHeight (Css.num 1.2)
                , Css.fontWeight (Css.int 700)
                ]
            , Heading.plaintext headerText
            ]
        , Html.div
            [ css
                [ Css.displayFlex
                , Css.flexWrap Css.wrap
                , Css.property "gap" "10px"
                ]
            ]
            (List.map (viewIcon showIconName) icons)
        ]


viewIcon : Bool -> ( String, Svg.Svg, List Css.Style ) -> Html msg
viewIcon showIconName ( name, icon, style ) =
    let
        iconCss =
            if List.isEmpty style then
                [ Css.height (Css.px 25)
                , Css.width (Css.px 25)
                , Css.margin (Css.px 4)
                , Css.color Colors.gray45
                ]

            else
                style
    in
    Html.div
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.backgroundColor Colors.gray96
            , Css.borderRadius (Css.px 8)
            , Css.padding2 (Css.px 5) (Css.px 10)
            , Css.hover
                [ Css.backgroundColor Colors.glacier
                , Css.color Colors.azure
                , Css.Global.descendants
                    [ Css.Global.selector "svg"
                        [ Css.color Colors.azure
                        ]
                    ]
                ]
            ]
        ]
        [ icon
            |> Svg.withCss iconCss
            |> Svg.toHtml
        , Text.smallBody
            [ Text.plaintext name
            , Text.css <|
                if showIconName then
                    []

                else
                    [ Css.display Css.none ]
            ]
        ]


viewSingularExampleSettings : List Group -> Settings -> Html.Html Msg
viewSingularExampleSettings groups state =
    let
        svgGroupedChoices ( groupName, items ) =
            let
                toEntry ( name, icon, _ ) =
                    Select.Choice name ( name, icon )
            in
            Select.ChoicesGroup groupName (List.map toEntry items)
    in
    Html.div
        [ Attributes.css
            [ Css.displayFlex
            , Css.justifyContent Css.spaceBetween
            , Css.alignItems Css.center
            , Css.flexWrap Css.wrap
            ]
        ]
        [ TextInput.view "Title"
            [ TextInput.value state.label
            , TextInput.text SetLabel
            ]
        , Select.view "Icon"
            [ Select.groupedChoices Tuple.first
                (List.map svgGroupedChoices groups)
            , Select.value (Just state.icon)
            ]
            |> Html.map SetIcon
        , Checkbox.view
            { label = "Show border"
            , selected = Checkbox.selectedFromBool state.showBorder
            }
            [ Checkbox.id "show-border"
            , Checkbox.onCheck SetBorder
            ]
        , Html.label []
            [ Html.text "Color: "
            , Html.input
                [ Attributes.type_ "color"
                , Attributes.value (SolidColor.toHex state.color)
                , Events.onInput (SetColor << SolidColor.fromHex)
                ]
                []
            ]
        , Html.label []
            [ Html.text "Width: "
            , Html.input
                [ Attributes.type_ "range"
                , Attributes.min "0"
                , Attributes.max "200"
                , Attributes.value (String.fromFloat state.width)
                , Events.onInput (SetWidth << String.toFloat)
                ]
                []
            ]
        , Html.label []
            [ Html.text "Height: "
            , Html.input
                [ Attributes.type_ "range"
                , Attributes.min "0"
                , Attributes.max "200"
                , Attributes.value (String.fromFloat state.height)
                , Events.onInput (SetHeight << String.toFloat)
                ]
                []
            ]
        ]


viewResults : Settings -> Html.Html Msg
viewResults state =
    let
        ( red, green, blue ) =
            SolidColor.toRGB state.color
    in
    Html.div
        [ Attributes.css
            [ Css.displayFlex
            , Css.property "gap" "20px"
            , Css.marginTop Spacing.verticalSpacerPx
            , Css.maxWidth (Css.px 700)
            , Css.flexWrap Css.wrap
            ]
        ]
        [ Html.pre [ Attributes.css [ Css.maxWidth (Css.px 400) ] ]
            [ [ Code.varWithTypeAnnotation "color" "Css.Color" <|
                    ("Css.rgb " ++ String.fromFloat red ++ " " ++ String.fromFloat green ++ " " ++ String.fromFloat blue)
              , Code.newlines
              , Code.varWithTypeAnnotation "renderedSvg" "Svg" <|
                    Code.pipelineMultiline
                        ([ Just <| state.renderSvgCode (Tuple.first state.icon)
                         , Just "Svg.withColor color"
                         , Just <| "Svg.withWidth (Css.px " ++ String.fromFloat state.width ++ ")"
                         , Just <| "Svg.withHeight (Css.px " ++ String.fromFloat state.height ++ ")"
                         , if state.showBorder then
                            Just
                                ("Svg.withCss"
                                    ++ Code.newlineWithIndent 3
                                    ++ "[ Css.border3 (Css.px 1) Css.solid Colors.gray20 ]"
                                )

                           else
                            Nothing
                         , if String.isEmpty state.label then
                            Nothing

                           else
                            Just ("Svg.withLabel " ++ Code.string state.label)
                         , Just "Svg.toHtml"
                         ]
                            |> List.filterMap identity
                        )
                        1
              ]
                |> String.join ""
                |> Html.text
            ]
        , Tuple.second state.icon
            |> Svg.withColor (toCssColor state.color)
            |> Svg.withWidth (Css.px state.width)
            |> Svg.withHeight (Css.px state.height)
            |> (\svg ->
                    if state.showBorder then
                        Svg.withCss [ Css.border3 (Css.px 1) Css.solid Colors.gray20 ] svg

                    else
                        svg
               )
            |> (\svg ->
                    if String.isEmpty state.label then
                        svg

                    else
                        Svg.withLabel state.label svg
               )
            |> Svg.toHtml
        ]
