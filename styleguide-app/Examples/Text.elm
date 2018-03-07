module Examples.Text exposing (example)

{-|

@docs example

-}

import Html as RootHtml
import Html.Styled as Html
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Text.V1 as Text


{-| -}
example : ModuleExample msg
example =
    { filename = "Nri.Ui.Text.V1.elm"
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
        [ Text.heading [ RootHtml.text "This is the main page heading." ]
        , Text.tagline [ RootHtml.text "This is a tagline" ]
        , Text.subHeading [ RootHtml.text "This is a subHeading" ]
        , Text.mediumBody [ RootHtml.text <| "This is a mediumBody. " ++ longerBody ]
        , Html.toUnstyled (Text.ugMediumBody [ Html.text <| "This is an ugMediumBody." ])
        , Text.smallBody [ RootHtml.text <| "This is a smallBody. " ++ longerBody ]
        , Text.smallBodyGray [ RootHtml.text <| "This is a smallBodyGray. " ++ longerBody ]
        , Html.toUnstyled (Text.ugSmallBody [ Html.text <| "This is an ugSmallBody." ])
        , Text.caption [ RootHtml.text <| "This is a caption. " ++ longerBody ]
        ]
    }
