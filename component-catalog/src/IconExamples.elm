module IconExamples exposing
    ( example
    , Settings, Msg
    , Group
    , preview, previewCustomSize
    )

{-| 🚨 If you update this module, please be sure that `script/add-example.sh` keeps working too.

@docs example
@docs Settings, Msg
@docs Group
@docs preview, previewCustomSize

-}

import Category exposing (Category(..))
import Code
import Css
import Css.Global
import Debug.Control.View exposing (viewWithCustomControls)
import EllieLink
import Example exposing (Example)
import ExampleSection
import Examples.Colors
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import InputErrorAndGuidanceInternal
import InputLabelInternal
import Nri.Ui.Checkbox.V7 as Checkbox
import Nri.Ui.Colors.Extra exposing (fromCssColor, toCssColor)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.CssVendorPrefix.V1 as VendorPrefixed
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.InputStyles.V4 as InputStyles
import Nri.Ui.Select.V9 as Select
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Text.V6 as Text
import Nri.Ui.TextInput.V8 as TextInput
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
    , init = init config
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview = config.preview
    , about = []
    , view = \ellieLinkConfig settings -> view config ellieLinkConfig settings config.all
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
view : { config | moduleName : String, version : Int } -> EllieLink.Config -> Settings -> List Group -> List (Html Msg)
view config ellieLinkConfig settings groups =
    let
        viewExampleSection ( group, values ) =
            viewWithCustomStyles settings group values

        ( code, renderedExample ) =
            toCustomizableExampleResults settings
    in
    ExampleSection.sectionWithCss "About" [ Css.flex (Css.int 1) ] Text.smallBody [ aboutMsg ]
        :: Html.section []
            [ viewWithCustomControls
                { ellieLinkConfig = ellieLinkConfig
                , name = config.moduleName
                , version = config.version
                , controls = viewCustomizableExample groups settings
                , mainType = Just "RootHtml.Html msg"
                , extraCode = []
                , renderExample = \renderedSvgDefinition -> "toUnstyled renderedSvg" ++ Code.newlines ++ renderedSvgDefinition
                , exampleCode =
                    [ { sectionName = config.moduleName
                      , code = code
                      }
                    ]
                }
            ]
        :: Heading.h2
            [ Heading.plaintext "Grouped Icons"
            , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
            ]
        :: viewSettings settings
        :: List.map viewExampleSection groups


aboutMsg : Text.Attribute msg
aboutMsg =
    Text.markdown
        """
- Our icons are Elm SVGs, not separate files or sprites. We use an opaque type to represent them, which enables nice type-safe composability across our components.
- For decorative SVGs (which is the default), we add `aria-hidden=true` to the SVG node.
- For non-decorative SVGs, we use Pattern #5 `<svg>` + `role='img'` + `<title>` from [Accessible SVGs: Perfect Patterns For Screen Reader Users](https://www.smashingmagazine.com/2021/05/accessible-svg-patterns-comparison/).
- Instructions for adding new SVG icons can be found in the [monolith README](https://github.com/NoRedInk/NoRedInk/blob/master/monolith/README.md#adding-new-svg-icons).

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


viewCustomizableExample : List Group -> Settings -> Html.Html Msg
viewCustomizableExample groups state =
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
        , Html.div [ css [ Css.position Css.relative, Css.paddingTop (Css.px InputStyles.defaultMarginTop) ] ]
            [ InputLabelInternal.view
                { for = "color-picker"
                , label = "Color"
                , theme = InputStyles.Standard
                }
                { error = InputErrorAndGuidanceInternal.noError
                , noMarginTop = False
                , hideLabel = False
                , disabled = False
                }
            , Html.input
                [ Attributes.id "color-picker"
                , Attributes.type_ "color"
                , Attributes.value (SolidColor.toHex state.color)
                , Events.onInput (SetColor << SolidColor.fromHex)
                , Attributes.list "noredink-ui-colors"
                , css
                    [ Css.border3 (Css.px 1) Css.solid Colors.gray75
                    , Css.borderBottomWidth (Css.px 3)
                    , Css.borderRadius (Css.px 8)
                    , Css.focus
                        [ Css.borderColor Colors.azure
                        , Css.borderRadius (Css.px 8) |> Css.important
                        ]
                    , Css.color Colors.gray20
                    , Css.backgroundColor Colors.white
                    , Css.minWidth (Css.px 94)
                    , Css.height (Css.px 45)
                    , Css.padding4 (Css.px 7) (Css.px 30) (Css.px 5) (Css.px 7)

                    -- Dropdown Arrow
                    --
                    -- "appearance: none" removes the default dropdown arrows
                    , VendorPrefixed.property "appearance" "none"
                    , """<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="16px" height="16px" viewBox="0 0 25 15"><g fill=" """
                        ++ SolidColor.toRGBString (fromCssColor Colors.azure)
                        ++ """ "><path transform="rotate(270) translate(-20)" d="M19.2677026,20.7322696 C20.2443584,21.7070736 20.2443584,23.2915005 19.2677026,24.2677859 C18.7788191,24.7555583 18.139567,25 17.4999444,25 C16.8603219,25 16.2210698,24.7555583 15.7321863,24.2677859 L5.73229742,14.267897 C4.7556416,13.293093 4.7556416,11.7086662 5.73229742,10.7323808 L15.7321863,0.732491861 C16.7084718,-0.244163954 18.2914171,-0.244163954 19.2677026,0.732491861 C20.2443584,1.70729584 20.2443584,3.29172268 19.2677026,4.26800813 L11.0359422,12.5001389 L19.2677026,20.7322696 Z" ></path></g></svg> """
                        |> urlUtf8
                        |> Css.property "background"
                    , Css.backgroundRepeat Css.noRepeat
                    , Css.property "background-position" "center right -20px"
                    , Css.backgroundOrigin Css.contentBox
                    ]
                ]
                []
            , Examples.Colors.all
                |> List.map (\( name, color ) -> Html.option [ Attributes.value color.value ] [])
                |> Html.datalist [ Attributes.id "noredink-ui-colors" ]
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


toCustomizableExampleResults : Settings -> ( String, Html msg )
toCustomizableExampleResults state =
    let
        ( red, green, blue ) =
            SolidColor.toRGB state.color

        selectedNriUiColor : Maybe String
        selectedNriUiColor =
            Examples.Colors.all
                |> List.filterMap
                    (\( name, color ) ->
                        if fromCssColor color == state.color then
                            Just name

                        else
                            Nothing
                    )
                |> List.head
    in
    ( (case selectedNriUiColor of
        Just color ->
            ""

        Nothing ->
            [ Code.varWithTypeAnnotation "color" "Css.Color" <|
                "Css.rgb "
                    ++ String.fromFloat red
                    ++ " "
                    ++ String.fromFloat green
                    ++ " "
                    ++ String.fromFloat blue
            , Code.newlines
            ]
                |> String.join ""
      )
        ++ (Code.varWithTypeAnnotation "renderedSvg" "Html msg" <|
                Code.pipelineMultiline
                    ([ Just <| state.renderSvgCode (Tuple.first state.icon)
                     , case selectedNriUiColor of
                        Just color ->
                            Just ("Svg.withColor Colors." ++ color)

                        Nothing ->
                            Just "Svg.withColor color"
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
           )
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
    )


urlUtf8 : String -> String
urlUtf8 content =
    """url('data:image/svg+xml;utf8,""" ++ content ++ """')"""
