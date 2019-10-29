module Examples.IconExamples exposing (view)

import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css, style, title)
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V4 as Text


view : String -> List ( String, Svg.Svg ) -> Html msg
view headerText icons =
    Html.section [ css [ Css.marginTop (Css.px 16) ] ]
        [ Heading.h2 [] [ Html.text headerText ]
        , Html.div [ css [ Css.displayFlex, Css.flexWrap Css.wrap ] ]
            (List.map viewIcon icons)
        ]


viewIcon : ( String, Svg.Svg ) -> Html msg
viewIcon ( name, icon ) =
    Html.div
        [ css
            [ Css.margin (Css.px 10)
            , Css.width (Css.px 160)
            , Css.boxShadow4 (Css.px 10) (Css.px 5) (Css.px 5) Colors.navy
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            , Css.justifyContent Css.flexStart
            ]
        ]
        [ Html.div
            [ css
                [ Css.height (Css.px 80)
                , Css.width (Css.px 80)
                , Css.margin (Css.px 10)
                , Css.color Colors.green
                ]
            ]
            [ Svg.toHtml icon
            ]
        , Text.smallBody [ Html.text name ]
        ]
