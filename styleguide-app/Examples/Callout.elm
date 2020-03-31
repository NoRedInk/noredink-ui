module Examples.Callout exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css
import Example exposing (Example)
import Html.Styled as Html
import Html.Styled.Attributes exposing (href, title)
import Nri.Ui.Callout.V1 as Callout exposing (callout)


type alias State =
    ()


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "Nri.Ui.Callout.V1"
    , categories = [ Messaging ]
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , view =
        \_ ->
            [ -- PLAIN
              Html.h3 [] [ Html.text "Originally Designed Use Case" ]
            , callout
                [ Callout.label (Html.text "BETA")
                , Callout.custom (title "beta warning")
                ]
                [ Html.text "This tab is still a work in progress; some of your student's information may be missing."
                , Html.br [] []
                , Html.text "To share your thoughts and help us improve the experience, "
                , Html.a [ href "#" ] [ Html.text "click here" ]
                , Html.text "."
                ]

            -- WITH SIDE TEXT
            , Html.h3 [] [ Html.text "Without side text" ]
            , callout
                [ Callout.custom (title "no side text") ]
                [ Html.text "I feel weird without my side text!" ]

            -- WITH CSS CUSTOMIZATIONS
            , Html.h3 [] [ Html.text "With CSS customizations" ]
            , callout
                [ Callout.containerCss [ Css.margin (Css.px 20) ]
                , Callout.label (Html.text "HMMM")
                , Callout.custom (title "margin")
                ]
                [ Html.text "My container styles are customized to add margin around the callout" ]
            , callout
                [ Callout.contentCss [ Css.textTransform Css.uppercase ]
                , Callout.label (Html.text "WOW!")
                , Callout.custom (title "yelling")
                ]
                [ Html.text "My content styles are customized to yell about everything" ]
            ]
    }
