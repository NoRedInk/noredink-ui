module Examples.Select exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled
import Html.Styled.Attributes
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Select.V8 as Select


{-| -}
example : Example State Msg
example =
    { name = "Select"
    , version = 8
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Inputs ]
    , keyboardSupport = []
    , view =
        \state ->
            [ Select.view "Tortilla Selector"
                { current = Nothing
                , choices =
                    [ { label = "Tacos", value = "Tacos" }
                    , { label = "Burritos", value = "Burritos" }
                    , { label = "Enchiladas", value = "Enchiladas" }
                    ]
                , valueToString = identity
                , defaultDisplayText = Just "Select a tasty tortilla based treat!"
                , isInError = False
                }
                []
                |> Html.Styled.map ConsoleLog
            , Select.view "Errored Selector"
                { current = Nothing
                , choices = []
                , valueToString = identity
                , defaultDisplayText = Just "Please select an option"
                , isInError = True
                }
                []
                |> Html.Styled.map ConsoleLog
            , Html.Styled.div
                [ Html.Styled.Attributes.css [ Css.maxWidth (Css.px 400) ] ]
                [ Select.view "Selector with Overflowed Text"
                    { current = Nothing
                    , choices = []
                    , valueToString = identity
                    , defaultDisplayText = Just "Look at me, I design coastlines, I got an award for Norway. Where's the sense in that?"
                    , isInError = False
                    }
                    []
                    |> Html.Styled.map ConsoleLog
                ]
            ]
    }


{-| -}
init : State
init =
    Nothing


{-| -}
type alias State =
    Maybe String


{-| -}
type Msg
    = ConsoleLog String


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ConsoleLog message ->
            let
                _ =
                    Debug.log "SelectExample" message
            in
            ( Just message, Cmd.none )
