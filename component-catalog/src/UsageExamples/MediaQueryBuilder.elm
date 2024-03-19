module UsageExamples.MediaQueryBuilder exposing (example)

{-|

    @docs example

-}

import Category
import Css exposing (backgroundColor, before, color, fontFamily, fontSize, property, px, sansSerif)
import Html.Styled exposing (..)
import Nri.Ui.Container.V2 as Container
import Nri.Ui.MediaQuery.V1 as MediaQuery
import UsageExample exposing (UsageExample)


example : UsageExample () ()
example =
    { name = "MediaQueryBuilder"
    , categories = [ Category.Layout ]
    , init = ()
    , update = \_ _ -> ( (), Cmd.none )
    , view = always view
    , about = []
    , subscriptions = \_ -> Sub.none
    }


view : List (Html msg)
view =
    let
        content str =
            before [ property "content" ("'" ++ str ++ "'") ]
    in
    [ h2 [] [ text "MediaQueryBuilder" ]
    , Container.view
        [ Container.buttony
        , Container.css
            (MediaQuery.builder
                [ content "I am the base style, visible to all devices"
                , fontFamily sansSerif
                ]
                |> MediaQuery.onNarrowMobile
                    [ content "I am the narrow mobile style, visible only to screens" ]
                |> MediaQuery.onQuizEngineMobile
                    [ content "I am the quiz engine mobile style, visible only to screens" ]
                |> MediaQuery.onMobile
                    [ content "I am the mobile style, visible only to screens" ]
                |> MediaQuery.onDesktop
                    [ content "I am the desktop style, visible only to screens" ]
                |> MediaQuery.toStyles
            )
        ]
    ]
