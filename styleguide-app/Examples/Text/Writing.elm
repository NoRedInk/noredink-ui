module Examples.Text.Writing exposing (example)

{-|

@docs example

-}

import Category exposing (Category(..))
import Html.Styled exposing (text)
import ModuleExample exposing (ModuleExample)
import Nri.Ui.Text.Writing.V1 as TextWriting
import Sort.Set as Set exposing (Set)


{-| -}
example : ModuleExample msg
example =
    { name = "Nri.Ui.Text.Writing.V1"
    , categories = Set.fromList Category.sorter <| List.singleton Text
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
