module Examples.IconExamples exposing
    ( preview
    , Settings, init, Msg, update, viewSettings
    , view, viewWithCustomStyles
    )

{-|

@docs preview
@docs Settings, init, Msg, update, viewSettings
@docs view, viewWithCustomStyles

-}

import Css
import Css.Global
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Checkbox.V5 as Checkbox
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V6 as Text


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
    { showIconName : Bool }


{-| -}
init : Settings
init =
    { showIconName = False }


{-| -}
type Msg
    = ShowNames Bool


{-| -}
update : Msg -> Settings -> ( Settings, Cmd msg )
update msg settings =
    case msg of
        ShowNames showIconName ->
            ( { settings | showIconName = showIconName }
            , Cmd.none
            )


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
