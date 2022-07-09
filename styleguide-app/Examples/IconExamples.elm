module Examples.IconExamples exposing
    ( preview
    , Settings, init, Msg, update, viewSettings
    , viewByGroupWithSettings, IconExampleGroup
    , view, viewWithCustomStyles
    )

{-|

@docs preview
@docs Settings, init, Msg, update, viewSettings
@docs viewByGroupWithSettings, IconExampleGroup
@docs view, viewWithCustomStyles

-}

import Css
import Css.Global
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Nri.Ui.Checkbox.V5 as Checkbox
import Nri.Ui.Colors.Extra exposing (fromCssColor, toCssColor)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Select.V8 as Select
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.Text.V6 as Text
import Nri.Ui.TextInput.V7 as TextInput
import Nri.Ui.UiIcon.V1 as UiIcon
import SolidColor exposing (SolidColor)


{-| -}
preview : List Svg.Svg -> List (Html msg)
preview icons =
    [ Html.div
        [ css
            [ Css.displayFlex
            , Css.flexWrap Css.wrap
            , Css.property "gap" "10px"
            , Css.color Colors.gray45
            ]
        ]
        (List.map
            (Svg.withWidth (Css.px 30) >> Svg.withHeight (Css.px 30) >> Svg.toHtml)
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
    }


{-| -}
init : Settings
init =
    { showIconName = False
    , iconSelectorExpanded = False
    , color = fromCssColor Colors.greenDark
    , width = 100
    , height = 100
    , -- TODO: use an appropriate example for each icon type
      icon = ( "starFilled", UiIcon.starFilled )
    , label = "Mastered"
    , showBorder = False
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
    Checkbox.viewWithLabel
        { identifier = "show-icon-name-checkbox"
        , label = "Show names"
        , setterMsg = ShowNames
        , selected = Checkbox.selectedFromBool showIconName
        , disabled = False
        , theme = Checkbox.Square
        }


type alias IconExampleGroup =
    ( String
    , List ( String, Svg.Svg )
    )


{-| -}
viewByGroupWithSettings : Settings -> List IconExampleGroup -> List (Html Msg)
viewByGroupWithSettings settings groups =
    let
        viewExampleSection ( group, values ) =
            view settings group values
    in
    viewSettings settings
        :: List.map viewExampleSection groups
        ++ [ Html.section [ css [ Css.margin2 (Css.px 30) Css.zero ] ]
                [ Heading.h3 [] [ Html.text "Example Usage" ]
                , viewSingularExampleSettings groups settings
                , viewResults settings
                ]
           ]


{-| -}
view : Settings -> String -> List ( String, Svg.Svg ) -> Html msg
view settings headerText icons =
    let
        defaultStyles =
            [ Css.height (Css.px 25)
            , Css.width (Css.px 25)
            , Css.margin (Css.px 4)
            , Css.color Colors.gray45
            ]
    in
    viewWithCustomStyles settings
        headerText
        (List.map (\( name, svg ) -> ( name, svg, defaultStyles )) icons)


{-| -}
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
            ]
            [ Html.text headerText ]
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
            |> Svg.withCss style
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


viewSingularExampleSettings : List IconExampleGroup -> Settings -> Html.Html Msg
viewSingularExampleSettings groups state =
    let
        svgGroupedChoices ( groupName, items ) =
            let
                toEntry ( name, icon ) =
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
        , Checkbox.viewWithLabel
            { identifier = "show-border"
            , label = "Show border"
            , setterMsg = SetBorder
            , selected = Checkbox.selectedFromBool state.showBorder
            , disabled = False
            , theme = Checkbox.Square
            }
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
    Html.div [ Attributes.css [ Css.displayFlex ] ]
        [ Html.pre
            [ Attributes.css
                [ Css.width (Css.px 400)
                , Css.marginRight (Css.px 20)
                ]
            ]
            [ [ "color : Css.Color\n"
              , "color =\n"
              , "    Css.rgb " ++ String.fromFloat red ++ " " ++ String.fromFloat green ++ " " ++ String.fromFloat blue
              , "\n\n\n"
              , "renderedSvg : Svg\n"
              , "renderedSvg =\n"
              , "   UiIcon." ++ Tuple.first state.icon ++ "\n"
              , "       |> Svg.withColor color\n"
              , "       |> Svg.withWidth (Css.px " ++ String.fromFloat state.width ++ ")\n"
              , "       |> Svg.withHeight (Css.px " ++ String.fromFloat state.height ++ ")\n"
              , if state.showBorder then
                    "       |> Svg.withCss [ Css.border3 (Css.px 1) Css.solid Colors.gray20 ]\n"

                else
                    ""
              , if String.isEmpty state.label then
                    ""

                else
                    "       |> Svg.withLabel \"" ++ state.label ++ "\"\n"
              , "       |> Svg.toHtml\n"
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
