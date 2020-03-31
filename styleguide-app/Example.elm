module Example exposing (Example, view)

import Category exposing (Category)
import Css exposing (..)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Nri.Ui.Colors.V1 exposing (..)
import Sort.Set as Set exposing (Set)


type alias Example state msg =
    { name : String
    , state : state
    , update : msg -> state -> ( state, Cmd msg )
    , subscriptions : state -> Sub msg
    , view : state -> List (Html msg)
    , categories : List Category
    }


view : Bool -> Example state msg -> Html msg
view showFocusLink example =
    Html.div
        [ -- this class makes the axe accessibility checking output easier to parse
          String.replace "." "-" example.name
            |> (++) "module-example__"
            |> Attributes.class
        ]
        [ Html.div
            [ Attributes.css
                [ displayFlex
                , alignItems center
                , justifyContent flexStart
                , flexWrap wrap
                , padding (px 4)
                , backgroundColor glacier
                ]
            ]
            [ Html.styled Html.h2
                [ color gray20
                , fontFamilies [ qt "Source Code Pro", "Consolas", "Courier", "monospace" ]
                , fontSize (px 20)
                , marginTop zero
                , marginBottom zero
                ]
                []
                [ Html.text example.name ]
            , String.replace "." "-" example.name
                |> (++) "https://package.elm-lang.org/packages/NoRedInk/noredink-ui/latest/"
                |> viewLink "view docs"
            , if showFocusLink then
                viewLink "see only this" ("#/doodad/" ++ example.name)

              else
                Html.text ""
            ]
        , Html.div [ Attributes.css [ padding2 (px 20) zero ] ] (example.view example.state)
        ]


viewLink : String -> String -> Html msg
viewLink text href =
    Html.a
        [ Attributes.href href
        , Attributes.css [ Css.display Css.block, marginLeft (px 20) ]
        ]
        [ Html.text text
        ]
