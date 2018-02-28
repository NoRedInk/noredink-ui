module Examples.Text.Quiz exposing (example)

{- \
   @docs example
-}

import Html
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Text.Quiz.V1 as TextQuiz


{-| -}
example : ModuleExample msg
example =
    { filename = "Nri.Ui.Text.Quiz.V1.elm"
    , category = TextQuiz
    , content =
        let
            longerBody =
                """Be on the lookout for a new and improved assignment
                    creation form! Soon, you'll be able to easily see a summary
                    of the content you're assigning, as well as an estimate for
                    how long the assignment will take.
                """
        in
        [ TextQuiz.footnote [ Html.text <| "This is a footnote. " ++ longerBody ]
        ]
    }
