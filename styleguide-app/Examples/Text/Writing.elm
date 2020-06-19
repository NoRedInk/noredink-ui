module Examples.Text.Writing exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Example exposing (Example)
import Html.Styled exposing (text)
import Nri.Ui.Text.Writing.V1 as TextWriting


{-| -}
type alias State =
    ()


{-| -}
type alias Msg =
    ()


example : Example State Msg
example =
    { name = "Nri.Ui.Text.Writing.V1"
    , categories = [ Text ]
    , atomicDesignType = AtomicDesignType.Atom
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
