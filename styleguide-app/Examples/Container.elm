module Examples.Container exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Text.V5 as Text


{-| -}
example : Example State Msg
example =
    { name = "Container"
    , version = 2
    , categories = [ Layout ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view =
        \state ->
            [ Heading.h3 [ Heading.css [ Css.marginTop (Css.px 8) ] ]
                [ Html.text "Default Container" ]
            , Html.text "Your go-to container."
            , Container.default [] (Html.text "Content, content...")
            , Heading.h3 [ Heading.css [ Css.marginTop (Css.px 8) ] ]
                [ Html.text "Gray Container" ]
            , Html.text "A container that doesn't draw too much attention to itself."
            , Container.gray [] (Html.text "Content, content...")
            , Heading.h3 [ Heading.css [ Css.marginTop (Css.px 8) ] ]
                [ Html.text "Pillow Container" ]
            , Html.text "When you want something big and soft."
            , Container.pillow [] (Html.text "Content, content...")
            , Heading.h3 [ Heading.css [ Css.marginTop (Css.px 8) ] ]
                [ Html.text "Buttony Container" ]
            , Html.text "Used for clickable button card things."
            , Container.buttony [] (Html.text "Content, content...")
            , Heading.h3 [ Heading.css [ Css.marginTop (Css.px 8) ] ]
                [ Html.text "Disabled Container" ]
            , Html.text "Used to indicate content is locked/inaccessible"
            , Container.disabled [] (Html.text "Content, content...")
            , Heading.h3 [ Heading.css [ Css.marginTop (Css.px 8) ] ]
                [ Html.text "Invalid Container" ]
            , Html.text "Used to indicate content is invalid"
            , Container.invalid [] (Html.text "Content, content...")
            ]
    }


{-| -}
init : State
init =
    {}


{-| -}
type alias State =
    {}


{-| -}
type alias Msg =
    ()


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    ( state, Cmd.none )
