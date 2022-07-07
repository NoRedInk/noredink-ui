module ViewHelpers exposing (viewExamples)

import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)


viewExamples : List ( String, Html msg ) -> Html msg
viewExamples examples =
    let
        viewExample ( name, view ) =
            Html.tr []
                [ Html.th [] [ Html.text name ]
                , Html.td [] [ view ]
                ]
    in
    Html.table [ css [ Css.width (Css.pct 100) ] ]
        [ Html.tbody [] (List.map viewExample examples)
        ]
