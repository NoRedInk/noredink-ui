module Debug.Control.View exposing (codeFromList, view)

import Css exposing (..)
import Css.Media exposing (withMedia)
import Debug.Control as Control exposing (Control)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.MediaQuery.V1 exposing (mobile)


{-| -}
view :
    { update : Control a -> msg
    , settings : Control a
    , toExampleCode : a -> List { sectionName : String, code : String }
    }
    -> Html msg
view config =
    let
        value =
            Control.currentValue config.settings
    in
    div
        [ css
            [ displayFlex
            , Css.flexWrap Css.wrap
            , Css.property "gap" "10px"
            , withMedia [ mobile ] [ flexDirection column, alignItems stretch ]
            ]
        ]
        [ viewSection "Settings" <|
            [ fromUnstyled (Control.view config.update config.settings) ]
        , viewExampleCode (config.toExampleCode value)
        ]


viewExampleCode : List { sectionName : String, code : String } -> Html msg
viewExampleCode values =
    viewSection "Generated Code" <|
        List.concatMap
            (\{ sectionName, code } ->
                [ Heading.h4 [] [ text sectionName ]
                , Html.Styled.code
                    [ css [ whiteSpace preWrap ]
                    ]
                    [ text code ]
                ]
            )
            values


viewSection : String -> List (Html msg) -> Html msg
viewSection name children =
    section [ css [ flex (int 1) ] ]
        (Heading.h3 [] [ text name ]
            :: children
        )


codeFromList : List ( String, a ) -> String
codeFromList list =
    "\n\t[ "
        ++ String.join "\n\t, " (List.map Tuple.first list)
        ++ "\n\t] "
