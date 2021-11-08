module Examples.IconExamples exposing (preview, view, viewWithCustomStyles)

import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css, style, title)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V6 as Text


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


view : String -> List ( String, Svg.Svg ) -> Html msg
view headerText icons =
    let
        defaultStyles =
            [ Css.height (Css.px 25)
            , Css.width (Css.px 25)
            , Css.margin (Css.px 4)
            , Css.color Colors.gray45
            ]
    in
    viewWithCustomStyles headerText
        (List.map (\( name, svg ) -> ( name, svg, defaultStyles )) icons)


viewWithCustomStyles : String -> List ( String, Svg.Svg, List Css.Style ) -> Html msg
viewWithCustomStyles headerText icons =
    Html.section [ css [ Css.displayFlex, Css.alignItems Css.center ] ]
        [ Html.div
            [ css
                [ Css.displayFlex
                , Css.flexWrap Css.wrap
                ]
            ]
            (List.map viewIcon icons)
        , Heading.h2
            [ Heading.css
                [ Css.marginLeft (Css.px 32)
                , Css.paddingLeft (Css.px 32)
                , Css.borderLeft3 (Css.px 2) Css.solid Colors.gray92
                , Css.fontSize (Css.px 16)
                ]
            ]
            [ Html.text headerText ]
        ]


viewIcon : ( String, Svg.Svg, List Css.Style ) -> Html msg
viewIcon ( name, icon, style ) =
    Html.div
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.center
            , Css.margin (Css.px 8)
            ]
        ]
        [ Html.div [ css style ] [ Svg.toHtml icon ]
        , Text.smallBody [ Text.plaintext name ]
        ]
