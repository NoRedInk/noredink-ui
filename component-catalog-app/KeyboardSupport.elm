module KeyboardSupport exposing
    ( view
    , KeyboardSupport
    , Key(..), Direction(..)
    )

{-|

@docs view
@docs KeyboardSupport
@docs Key, Direction

-}

import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V3 as Heading


{-| -}
type alias KeyboardSupport =
    { keys : List Key
    , result : String
    }


{-| -}
view : List KeyboardSupport -> Html msg
view keyboardSupport =
    case keyboardSupport of
        [] ->
            text ""

        _ ->
            Container.view
                [ Container.html
                    [ Heading.h2 [ Heading.plaintext "Keyboard Support" ]
                    , ul
                        [ css [ listStyle none, margin2 (px 10) zero, padding zero ]
                        ]
                        (List.map viewKeyboardActions keyboardSupport)
                    ]
                ]
                |> List.singleton
                |> aside []


viewKeyboardActions : KeyboardSupport -> Html msg
viewKeyboardActions { keys, result } =
    li []
        [ strong [] [ text (String.join "+" (List.map keyToString keys) ++ ": ") ]
        , text result
        ]


{-| -}
type Key
    = Shift
    | Enter
    | Arrow Direction
    | Tab
    | Space
    | Esc


keyToString : Key -> String
keyToString key =
    case key of
        Shift ->
            "Shift"

        Enter ->
            "Enter"

        Arrow direction ->
            directionToString direction ++ " arrow"

        Tab ->
            "Tab"

        Space ->
            "Space"

        Esc ->
            "Escape"


{-| -}
type Direction
    = Up
    | Right
    | Down
    | Left


directionToString : Direction -> String
directionToString direction =
    case direction of
        Up ->
            "Up"

        Right ->
            "Right"

        Down ->
            "Down"

        Left ->
            "Left"
