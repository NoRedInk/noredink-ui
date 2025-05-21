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
import ExampleSection
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Fonts.V1 as Fonts
import String


{-| -}
type alias KeyboardSupport =
    { keys : List Key
    , result : String
    }


{-| -}
view : List KeyboardSupport -> Html msg
view =
    ExampleSection.asideWithCss "Keyboard Support"
        [ flex (int 1) ]
        viewKeyboardActionsDl


viewKeyboardActionsDl : List KeyboardSupport -> Html msg
viewKeyboardActionsDl keyboardSupport =
    dl
        [ css [ listStyle none, margin2 (px 10) zero, padding zero, Fonts.baseFont ]
        ]
        (List.concatMap viewKeyboardActions keyboardSupport)


viewKeyboardActions : KeyboardSupport -> List (Html msg)
viewKeyboardActions { keys, result } =
    if List.isEmpty keys then
        case String.split ":" result of
            keyPart :: rest ->
                let
                    descriptionText =
                        String.trim (String.join ":" rest)
                in
                if String.isEmpty descriptionText then
                    [ dt [ css [ Css.fontWeight Css.bold, Css.marginBottom (Css.px 8) ] ] [ text keyPart ] ]

                else
                    [ dt [ css [ Css.fontWeight Css.bold ] ] [ text (keyPart ++ ":") ]
                    , dd [ css [ Css.margin Css.zero, Css.marginBottom (Css.px 8) ] ] [ text descriptionText ]
                    ]

            _ ->
                [ dt [ css [ Css.fontWeight Css.bold, Css.marginBottom (Css.px 8) ] ] [ text result ] ]

    else
        [ dt [ css [ Css.fontWeight Css.bold ] ] [ text (String.join "+" (List.map keyToString keys) ++ ":") ]
        , dd [ css [ Css.margin Css.zero, Css.marginBottom (Css.px 8) ] ] [ text result ]
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
