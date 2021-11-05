module Examples.Container exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import CommonControls exposing (romeoAndJulietQuotation)
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Container.V2 as Container
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V6 as Text
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
    , preview = []
    , view =
        \state ->
            let
                attributes =
                    Control.currentValue state.control
            in
            [ Control.view UpdateControl state.control
                |> Html.fromUnstyled
            , viewExample
                { name = "Default Container"
                , description = "Your go-to container."
                }
                (Container.default :: attributes)
            , viewExample
                { name = "Gray Container"
                , description = "A container that doesn’t draw too much attention to itself."
                }
                (Container.gray :: attributes)
            , viewExample
                { name = "Pillow Container"
                , description = "When you want something big and soft."
                }
                (Container.pillow :: attributes)
            , viewExample
                { name = "Buttony Container"
                , description = "Used for clickable button card things."
                }
                (Container.buttony :: attributes)
            , viewExample
                { name = "Disabled Container"
                , description = "Used to indicate content is locked/inaccessible"
                }
                (Container.disabled :: attributes)
            , viewExample
                { name = "Invalid Container"
                , description = "Used to indicate content is invalid"
                }
                (Container.invalid :: attributes)
            ]
    }


viewExample : { name : String, description : String } -> List (Container.Attribute msg) -> Html msg
viewExample { name, description } attributes =
    Html.section
        [ css
            [ Css.marginTop (Css.px 20)
            ]
        ]
        [ Heading.h3 [] [ Html.text name ]
        , Html.text description
        , Container.view attributes
        ]


{-| -}
type alias State =
    { control : Control (List (Container.Attribute Msg))
    }


{-| -}
init : State
init =
    { control =
        ControlExtra.list
            |> ControlExtra.optionalListItem "paddingPx" controlPaddingPx
            |> ControlExtra.optionalListItem "css" controlCss
            |> ControlExtra.listItem "content" controlContent
    }


controlPaddingPx : Control (Container.Attribute msg)
controlPaddingPx =
    Control.map Container.paddingPx (ControlExtra.float 20)


controlCss : Control (Container.Attribute msg)
controlCss =
    Control.map Container.css
        (Control.value
            [ Css.minHeight (Css.px 100)
            , Css.hover [ Css.backgroundColor Colors.glacier ]
            ]
        )


controlContent : Control (Container.Attribute msg)
controlContent =
    Control.choice
        [ ( "plain text (short)"
          , Control.string "Content, content..."
                |> Control.map Container.plaintext
          )
        , ( "plain text (long)"
          , Control.stringTextarea romeoAndJulietQuotation
                |> Control.map Container.plaintext
          )
        , ( "markdown"
          , Control.string romeoAndJulietQuotation
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
