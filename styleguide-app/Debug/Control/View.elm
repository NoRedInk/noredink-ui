module Debug.Control.View exposing (codeFromList, view)

import Css exposing (..)
import Debug.Control as Control exposing (Control)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.Heading.V2 as Heading


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
    div [ css [ displayFlex ] ]
        [ fromUnstyled (Control.view config.update config.settings)
        , viewExampleCode (config.toExampleCode value)
        ]


viewExampleCode : List { sectionName : String, code : String } -> Html msg
viewExampleCode values =
    section []
        (Heading.h3 [] [ text "Generated Code" ]
            :: List.concatMap
                (\{ sectionName, code } ->
                    [ Heading.h4 [] [ text sectionName ]
                    , Html.Styled.code [] [ text code ]
                    ]
                )
                values
        )


codeFromList : List ( String, a ) -> String
codeFromList list =
    "\n\t[ "
        ++ String.join "\n\t, " (List.map Tuple.first list)
        ++ "\n\t] "
