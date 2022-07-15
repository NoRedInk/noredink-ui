module Examples.Fonts exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css exposing (Style)
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading


{-| -}
type alias State =
    ()


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "Fonts"
    , version = 1
    , categories = [ Text, Atoms ]
    , keyboardSupport = []
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
    , subscriptions = \_ -> Sub.none
    , preview =
        [ ( "baseFont", Fonts.baseFont )
        , ( "quizFont", Fonts.quizFont )
        , ( "ugFont", Fonts.ugFont )
        ]
            |> List.map viewPreview
    , view =
        \ellieLinkConfig _ ->
            [ Heading.h2 [ Heading.style Heading.Subhead ] [ Html.text "baseFont" ]
            , Html.p [ css [ Fonts.baseFont ] ]
                [ Html.text "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz" ]
            , Heading.h2 [ Heading.style Heading.Subhead ] [ Html.text "quizFont" ]
            , Html.p [ css [ Fonts.quizFont ] ]
                [ Html.text "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz" ]
            , Heading.h2 [ Heading.style Heading.Subhead ] [ Html.text "ugFont" ]
            , Html.p [ css [ Fonts.ugFont ] ]
                [ Html.text "AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRrSsTtUuVvWwXxYyZz" ]
            ]
    }


viewPreview : ( String, Style ) -> Html msg
viewPreview ( name, font ) =
    Html.div
        [ css
            [ Css.displayFlex
            , Css.justifyContent Css.spaceBetween
            , font
            , Css.fontSize (Css.px 14)
            ]
        ]
        [ Html.p [ css [ Css.margin2 (Css.px 8) Css.zero ] ]
            [ Html.text name ]
        , Html.p [ css [ Css.margin2 (Css.px 8) Css.zero ] ]
            [ Html.text "AaBbCc" ]
        ]
