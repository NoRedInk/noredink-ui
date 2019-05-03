module Examples.Text.Writing exposing (example)

{- \
   @docs example
-}

import Html.Styled exposing (text)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Text.Writing.V1 as TextWriting


{-| -}
example : ModuleExample msg
example =
    { filename = "Nri.Ui.Text.Writing.V1.elm"
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
        [ TextWriting.footnote [ text <| "This is a footnote. " ++ longerBody ]
        ]
    }
