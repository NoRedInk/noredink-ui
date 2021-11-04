module Examples.Select exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Example exposing (Example)
import Html.Styled
import Html.Styled.Attributes
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Select.V8 as Select exposing (Choice)


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
            let
                attributes : Settings
                attributes =
                    Control.currentValue state.attributes
            in
            [ Control.view UpdateAttributes state.attributes
                |> Html.Styled.fromUnstyled
            , Select.view "Tortilla Selector" attributes
                |> Html.Styled.map ConsoleLog
            , Html.Styled.div
                [ Html.Styled.Attributes.css [ Css.maxWidth (Css.px 400) ] ]
                [ Select.view "Selector with Overflowed Text"
                    [ Select.defaultDisplayText "Look at me, I design coastlines, I got an award for Norway. Where's the sense in that?"
                    ]
                    |> Html.Styled.map ConsoleLog
                ]
            ]
    }


{-| -}
type alias State =
    { attributes : Control Settings
    }


{-| -}
init : State
init =
    { attributes = initControls
    }


type alias Settings =
    List (Select.Attribute String)


initControls : Control Settings
initControls =
    ControlExtra.list
        |> ControlExtra.listItem "choices"
            (Control.map (Select.choices identity) initChoices)
        |> ControlExtra.optionalListItem "defaultDisplayText"
            (Control.map Select.defaultDisplayText <|
                Control.string "Select a tasty tortilla based treat!"
            )
        |> ControlExtra.optionalListItem "errorIf"
            (Control.map Select.errorIf <| Control.bool True)
        |> ControlExtra.optionalListItem "errorMessage"
            (Control.map (Just >> Select.errorMessage) <|
                Control.string "The right item must be selected."
            )


initChoices : Control (List (Choice String))
initChoices =
    ControlExtra.list
        |> initChoice "value-1" "Tacos"
        |> initChoice "value-2" "Burritos"
        |> initChoice "value-3" "Enchiladas"


initChoice : String -> String -> Control (List (Choice String)) -> Control (List (Choice String))
initChoice value default =
    let
        toChoice label =
            { label = label, value = value }
    in
    ControlExtra.listItem value (Control.map toChoice (Control.string default))


{-| -}
type Msg
    = ConsoleLog String
    | UpdateAttributes (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        ConsoleLog message ->
            let
                _ =
                    Debug.log "SelectExample" message
            in
            ( state, Cmd.none )

        UpdateAttributes attributes ->
            ( { state | attributes = attributes }
            , Cmd.none
            )
