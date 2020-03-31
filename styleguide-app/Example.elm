module Example exposing (Example, view, wrap)

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


wrap :
    { wrapMsg : msg -> msg2
    , unwrapMsg : msg2 -> Maybe msg
    , wrapState : state -> state2
    , unwrapState : state2 -> Maybe state
    }
    -> Example state msg
    -> Example state2 msg2
wrap { wrapMsg, unwrapMsg, wrapState, unwrapState } example =
    { name = example.name
    , state = wrapState example.state
    , update =
        \msg2 state2 ->
            case ( unwrapMsg msg2, unwrapState state2 ) of
                ( Just msg, Just state ) ->
                    example.update msg state
                        |> Tuple.mapFirst wrapState
                        |> Tuple.mapSecond (Cmd.map wrapMsg)

                _ ->
                    ( state2, Cmd.none )
    , subscriptions =
        \state2 ->
            case unwrapState state2 of
                Just state ->
                    Sub.map wrapMsg (example.subscriptions state)

                Nothing ->
                    Sub.none
    , view =
        \state2 ->
            case unwrapState state2 of
                Just state ->
                    List.map (Html.map wrapMsg) (example.view state)

                Nothing ->
                    []
    , categories = example.categories
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
                , flexWrap Css.wrap
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
