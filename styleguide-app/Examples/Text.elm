module Examples.Text exposing (example)

{-|

@docs example

-}

import Html.Styled as Html
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Text.V2 as Text


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.Text.V2"
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
        [ Text.heading [ Html.text "This is the main page heading." ]
        , Text.tagline [ Html.text "This is a tagline" ]
        , Text.subHeading [ Html.text "This is a subHeading" ]
        , Text.smallHeading [ Html.text "This is a smallHeading" ]
        , Text.mediumBody [ Html.text <| "This is a mediumBody. " ++ longerBody ]
        , Text.ugMediumBody [ Html.text <| "This is an ugMediumBody." ]
        , Text.smallBody [ Html.text <| "This is a smallBody. " ++ longerBody ]
        , Text.smallBodyGray [ Html.text <| "This is a smallBodyGray. " ++ longerBody ]
        , Text.ugSmallBody [ Html.text <| "This is an ugSmallBody." ]
        , Text.caption [ Html.text <| "This is a caption. " ++ longerBody ]
        ]
    }
