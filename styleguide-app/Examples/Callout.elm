module Examples.Callout exposing (example)

{-|

@docs example

-}

import Css
import Html.Styled as Html
import Html.Styled.Attributes exposing (href, title)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Callout.V1 as Callout exposing (callout)


example : ModuleExample msg
example =
    { name = "Nri.Ui.Callout.V1"
    , category = Messaging
    , content =
        [ -- PLAIN
          Html.h3 [] [ Html.text "Originally Designed Use Case" ]
        , callout
            [ Callout.sideText (Html.text "BETA")
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
            , Callout.sideText (Html.text "HMMM")
            , Callout.custom (title "margin")
            ]
            [ Html.text "My container styles are customized to add margin around the callout" ]
        , callout
            [ Callout.contentCss [ Css.textTransform Css.uppercase ]
            , Callout.sideText (Html.text "WOW!")
            , Callout.custom (title "yelling")
            ]
            [ Html.text "My content styles are customized to yell about everything" ]
        ]
    }
