module HeadingsStyled exposing (h1, h2, h3, h4, h5)

import Html.Styled exposing (Html)
import Nri.Ui.Text.V2 as Text


h1 : List (Html msg) -> Html msg
h1 =
    Text.heading


h2 : List (Html msg) -> Html msg
h2 =
    Text.heading


h3 : List (Html msg) -> Html msg
h3 =
    Text.subHeading


h4 : List (Html msg) -> Html msg
h4 =
    Text.subHeading


h5 : List (Html msg) -> Html msg
h5 =
    Text.subHeading
