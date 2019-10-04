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
    { name = "Nri.Ui.Text.V4"
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
        [ Text.caption [ Html.text "NOTE: When using these styles, please read the documentation in the Elm module about \"Understanding spacing\"" ]
        , Heading.h2 [ Heading.style Heading.Top ] [ Html.text "Paragraph styles" ]
        , Text.mediumBody [ Html.text <| "This is a mediumBody. " ++ longerBody ]
        , Text.smallBody [ Html.text <| "This is a smallBody. " ++ longerBody ]
        , Text.smallBodyGray [ Html.text <| "This is a smallBodyGray. " ++ longerBody ]
        , Text.caption [ Html.text <| "This is a caption. " ++ longerBody ]
        , Heading.h2 [ Heading.style Heading.Top ] [ Html.text "Paragraph styles for user-authored content" ]
        , Text.ugMediumBody [ Html.text <| "This is an ugMediumBody. " ++ longerBody ]
        , Text.ugSmallBody [ Html.text <| "This is an ugSmallBody. " ++ longerBody ]
        ]
    }
