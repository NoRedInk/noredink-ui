module Examples.Container exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Css
import Debug.Control as Control exposing (Control)
import Example exposing (Example)
import Html.Styled as Html
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V5 as Text
import Nri.Ui.UiIcon.V1 as UiIcon


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
            let
                attributes =
                    Control.currentValue state.control
            in
            [ Control.view UpdateControl state.control
                |> Html.fromUnstyled
            , Heading.h3 [ Heading.css [ Css.marginTop (Css.px 8) ] ]
                [ Html.text "Default Container" ]
            , Html.text "Your go-to container."
            , Container.view (Container.default :: attributes)
            , Heading.h3 [ Heading.css [ Css.marginTop (Css.px 8) ] ]
                [ Html.text "Gray Container" ]
            , Html.text "A container that doesn't draw too much attention to itself."
            , Container.view (Container.gray :: attributes)
            , Heading.h3 [ Heading.css [ Css.marginTop (Css.px 8) ] ]
                [ Html.text "Pillow Container" ]
            , Html.text "When you want something big and soft."
            , Container.view (Container.pillow :: attributes)
            , Heading.h3 [ Heading.css [ Css.marginTop (Css.px 8) ] ]
                [ Html.text "Buttony Container" ]
            , Html.text "Used for clickable button card things."
            , Container.view (Container.buttony :: attributes)
            , Heading.h3 [ Heading.css [ Css.marginTop (Css.px 8) ] ]
                [ Html.text "Disabled Container" ]
            , Html.text "Used to indicate content is locked/inaccessible"
            , Container.view (Container.disabled :: attributes)
            , Heading.h3 [ Heading.css [ Css.marginTop (Css.px 8) ] ]
                [ Html.text "Invalid Container" ]
            , Html.text "Used to indicate content is invalid"
            , Container.view (Container.invalid :: attributes)
            ]
    }


{-| -}
type alias State =
    { control : Control (List (Container.Attribute Msg))
    }


{-| -}
init : State
init =
    { control =
        Control.record List.singleton
            |> Control.field "content" controlContent
    }


controlContent : Control (Container.Attribute msg)
controlContent =
    Control.choice
        [ ( "plain text (short)"
          , Control.string "Content, content..."
                |> Control.map Container.plaintext
          )
        , ( "plain text (long)"
          , Control.stringTextarea "TODO: Think of some good long plain text content!"
                |> Control.map Container.plaintext
          )
        , ( "markdown"
          , Control.string "TODO: Think of some good markdown content!"
                |> Control.map Container.markdown
          )
        , ( "HTML (short)"
          , Control.value
                (Container.html
                    [ UiIcon.footsteps
                        |> Svg.withHeight (Css.px 200)
                        |> Svg.withColor Colors.grassland
                        |> Svg.toHtml
                    ]
                )
          )
        ]


{-| -}
type Msg
    = UpdateControl (Control (List (Container.Attribute Msg)))


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateControl newControl ->
            ( { state | control = newControl }, Cmd.none )
