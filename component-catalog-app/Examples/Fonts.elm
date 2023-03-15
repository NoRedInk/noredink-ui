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
import Nri.Ui.Table.V6 as Table


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
    , view = \ellieLinkConfig _ -> [ view ]
    }


view : Html msg
view =
    let
        fontStyle font _ =
            [ font
            , Css.whiteSpace Css.pre
            , Css.textAlign Css.center
            ]
    in
    Table.view
        [ Table.rowHeader
            { header = Html.text "Example"
            , view = Html.text << .example
            , width = Css.pct 10
            , cellStyles = always [ Fonts.baseFont, Css.textAlign Css.left ]
            , sort = Nothing
            }
        , Table.string
            { header = "baseFont"
            , value = .text
            , width = Css.pct 10
            , cellStyles = fontStyle Fonts.baseFont
            , sort = Nothing
            }
        , Table.string
            { header = "quizFont/ugFont"
            , value = .text
            , width = Css.pct 10
            , cellStyles = fontStyle Fonts.quizFont
            , sort = Nothing
            }
        , Table.string
            { header = "Info"
            , value = .info
            , width = Css.pct 30
            , cellStyles = always [ Fonts.baseFont, Css.padding2 (Css.px 8) Css.zero ]
            , sort = Nothing
            }
        ]
        [ { example = "Alphabet", text = "AaBbCcDdEeFfGg\nHhIiJjKkLlMmNn\nOoPpQqRrSsTtUu\nVvWwXxYyZz", info = "" }
        , { example = "Imposter letters", text = "Il1 ecoa", info = "Note that capital i and lowercase l can look near-identical in some fonts. e, c, o, and a can also be difficult to distinguish from each other in some fonts." }
        , { example = "Mirrored letters", text = "db\nqp", info = "Mirrored letters are not unique, and can make reading more difficult, particularly for some people with dyslexia." }
        , { example = "Letter spacing", text = "rn vv", info = "r and n next to each other can smoosh into an m shape and v and v into a w in some fonts." }
        ]


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
