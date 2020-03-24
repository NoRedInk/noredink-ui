module Examples.Select exposing
    ( Msg
    , State
    , example
    , init
    , update
    )

{-|

@docs Msg
@docs State
@docs example
@docs init
@docs update

-}

import Category exposing (Category(..))
import Css
import Html.Styled
import Html.Styled.Attributes
import ModuleExample exposing (ModuleExample)
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Select.V7 as Select


{-| -}
example : (Msg -> msg) -> State -> ModuleExample msg
example parentMessage state =
    { name = "Nri.Ui.Select.V7"
    , category = Inputs
    , content =
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
            |> Html.Styled.map (parentMessage << ConsoleLog)
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
            |> Html.Styled.map (parentMessage << ConsoleLog)
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
                |> Html.Styled.map (parentMessage << ConsoleLog)
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
