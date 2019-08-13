module Examples.Text exposing (example)

{-|

@docs example

-}

import Html.Styled as Html
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Text.V4 as Text


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.Text.V4 (with headers from Nri.Ui.Heading.V2)"
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
        , Heading.h1 [] [ Html.text "This is the main page heading." ]
        , Heading.h2 [] [ Html.text "This is a tagline" ]
        , Heading.h3 [] [ Html.text "This is a subHeading" ]
        , Heading.h4 [] [ Html.text "This is a smallHeading" ]
        , Html.hr [] []
        , Heading.h2 [] [ Html.text "Paragraph styles" ]
        , Text.mediumBody [ Html.text <| "This is a mediumBody. " ++ longerBody ]
        , Text.smallBody [ Html.text <| "This is a smallBody. " ++ longerBody ]
        , Text.smallBodyGray [ Html.text <| "This is a smallBodyGray. " ++ longerBody ]
        , Text.caption [ Html.text <| "This is a caption. " ++ longerBody ]
        , Html.hr [] []
        , Heading.h2 [] [ Html.text "Paragraph styles for user-authored content" ]
        , Text.ugMediumBody [ Html.text <| "This is an ugMediumBody. " ++ longerBody ]
        , Text.ugSmallBody [ Html.text <| "This is an ugSmallBody. " ++ longerBody ]
        ]
    }
