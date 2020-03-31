module Examples.Text.Writing exposing (example)

{-|

@docs example

-}

import Category exposing (Category(..))
import Example exposing (Example)
import Html.Styled exposing (text)
import Nri.Ui.Text.Writing.V1 as TextWriting


{-| -}
example : Example () ()
example =
    { name = "Nri.Ui.Text.Writing.V1"
    , categories = List.singleton Text
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \_ ->
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
