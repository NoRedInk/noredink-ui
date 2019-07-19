module Examples.Text exposing (example)

{-|

@docs example

-}

import Html.Styled as Html
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Heading.V1 as Heading exposing (DocumentLevel(..), VisualLevel(..), heading, withDocumentLevel, withVisualLevel)
import Nri.Ui.Text.V3 as Text


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.Text.V3 (with headers from Nri.Ui.Heading.V1)"
    , category = Text
    , content =
        let
            longerBody =
                """Be on the lookout for a new and improved assignment
                    creation form! Soon, you'll be able to easily see a summary
                    of the content you're assigning, as well as an estimate for
                    how long the assignment will take.
                """
        in
        [ Html.text "NOTE: When using these styles, please read the documentation in the Elm module about \"Understanding spacing\""
        , heading [ Html.text "This is the main page heading." ]
            |> withVisualLevel Top
            |> withDocumentLevel H1
            |> Heading.view
        , heading [ Html.text "This is a tagline" ]
            |> withVisualLevel Tagline
            |> withDocumentLevel H2
            |> Heading.view
        , heading [ Html.text "This is a subHeading" ]
            |> withVisualLevel Subhead
            |> withDocumentLevel H3
            |> Heading.view
        , heading [ Html.text "This is a smallHeading" ]
            |> withVisualLevel Small
            |> withDocumentLevel H4
            |> Heading.view
        , Html.hr [] []
        , Text.heading [ Html.text "Paragraph styles" ]
        , Text.mediumBody [ Html.text <| "This is a mediumBody. " ++ longerBody ]
        , Text.smallBody [ Html.text <| "This is a smallBody. " ++ longerBody ]
        , Text.smallBodyGray [ Html.text <| "This is a smallBodyGray. " ++ longerBody ]
        , Text.caption [ Html.text <| "This is a caption. " ++ longerBody ]
        , Html.hr [] []
        , Text.heading [ Html.text "Paragraph styles for user-authored content" ]
        , Text.ugMediumBody [ Html.text <| "This is an ugMediumBody. " ++ longerBody ]
        , Text.ugSmallBody [ Html.text <| "This is an ugSmallBody. " ++ longerBody ]
        ]
    }
