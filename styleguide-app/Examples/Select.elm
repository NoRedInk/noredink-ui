module Examples.Select exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled
import Html.Styled.Attributes
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Select.V7 as Select


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.Select.V7"
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , categories = [ Inputs ]
    , atomicDesignType = Molecule
    , keyboardSupport = []
    , view =
        \state ->
            [ Html.Styled.label
                [ Html.Styled.Attributes.for "tortilla-selector" ]
                [ Heading.h3 [] [ Html.Styled.text "Tortilla Selector" ] ]
            , Select.view
                { current = Nothing
                , choices =
                    [ { label = "Tacos", value = "Tacos" }
                    , { label = "Burritos", value = "Burritos" }
                    , { label = "Enchiladas", value = "Enchiladas" }
                    ]
                , id = "tortilla-selector"
                , valueToString = identity
                , defaultDisplayText = Just "Select a tasty tortilla based treat!"
                , isInError = False
                }
                |> Html.Styled.map ConsoleLog
            , Html.Styled.label
                [ Html.Styled.Attributes.for "errored-selector" ]
                [ Heading.h3 [] [ Html.Styled.text "Errored Selector" ] ]
            , Select.view
                { current = Nothing
                , choices = []
                , id = "errored-selector"
                , valueToString = identity
                , defaultDisplayText = Just "Please select an option"
                , isInError = True
                }
                |> Html.Styled.map ConsoleLog
            , Html.Styled.label
                [ Html.Styled.Attributes.for "overflowed-selector" ]
                [ Heading.h3 [] [ Html.Styled.text "Selector with Overflowed Text" ] ]
            , Html.Styled.div
                [ Html.Styled.Attributes.css [ Css.maxWidth (Css.px 400) ] ]
                [ Select.view
                    { current = Nothing
                    , choices = []
                    , id = "overflowed-selector"
                    , valueToString = identity
                    , defaultDisplayText = Just "Look at me, I design coastlines, I got an award for Norway. Where's the sense in that?"
                    , isInError = False
                    }
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
