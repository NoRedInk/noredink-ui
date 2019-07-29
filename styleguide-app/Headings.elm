module Headings exposing (h1, h2, h3, h4, h5)

import Html.Styled exposing (Html)
import Nri.Ui.Heading.V1 as Headings exposing (..)
import Nri.Ui.Text.V4 as Text


h1 : List (Html msg) -> Html msg
h1 =
    heading
        >> withVisualLevel Top
        >> withDocumentLevel H1
        >> view


h2 : List (Html msg) -> Html msg
h2 =
    heading
        >> withVisualLevel Top
        >> withDocumentLevel H2
        >> view


h3 : List (Html msg) -> Html msg
h3 =
    heading
        >> withVisualLevel Subhead
        >> withDocumentLevel H3
        >> view


h4 : List (Html msg) -> Html msg
h4 =
    heading
        >> withVisualLevel Subhead
        >> withDocumentLevel H4
        >> view


h5 : List (Html msg) -> Html msg
h5 =
    heading
        >> withVisualLevel Subhead
        >> withDocumentLevel H5
        >> view
