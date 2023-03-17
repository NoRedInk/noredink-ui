module Examples.Fonts exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css exposing (Style)
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Table.V6 as Table
import Nri.Ui.Text.V6 as Text


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
            let
                dt =
                    Html.dt [ css [ Css.fontWeight Css.bold ] ]

                dd =
                    Html.dd [ css [ Css.marginLeft Css.zero, Css.marginBottom (Css.px 8) ] ]
            in
            [ Html.dl [ css [ Fonts.baseFont ] ]
                [ dt [ Html.text "quizFont" ]
                , dd [ Html.text "Use for exercise content. Georgia" ]
                , dt [ Html.text "ugFont" ]
                , dd [ Html.text "Use for user-generated content. Georgia" ]
                , dt [ Html.text "baseFont" ]
                , dd [ Html.text "Use  for everything else! Mulish" ]
                ]
            , viewFontFailurePatterns
            , Text.mediumBody
                [ Text.css [ Css.marginTop (Css.px 30) |> Css.important ]
                , Text.html
                    [ Html.text "Learn more about kid-friendly and accessible fonts starting at 24:40 in "
                    , ClickableText.link "Kids Websites: Where Fun and Accessibility Come to Play"
                        [ ClickableText.linkExternal "https://www.deque.com/axe-con/sessions/kids-websites-where-fun-and-accessibility-come-to-play/"
                        , ClickableText.appearsInline
                        ]
                    , Html.text " and in "
                    , ClickableText.link "Accessible fonts and readability: the basics"
                        [ ClickableText.linkExternal "https://business.scope.org.uk/article/font-accessibility-and-readability-the-basics#:~:text=This%20can%20affect%20reading%20speed,does%20this%20is%20Gill%20Sans."
                        , ClickableText.appearsInline
                        ]
                    , Html.text "."
                    ]
                ]
            ]
    }


viewFontFailurePatterns : Html msg
viewFontFailurePatterns =
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
            , cellStyles = always [ Fonts.baseFont, Css.padding2 (Css.px 8) Css.zero, Css.whiteSpace Css.preLine ]
            , sort = Nothing
            }
        ]
        [ { example = "Alphabet", text = "AaBbCcDdEeFfGg\nHhIiJjKkLlMmNn\nOoPpQqRrSsTtUu\nVvWwXxYyZz", info = "" }
        , { example = "Imposter letters", text = "Il1 ecoa", info = "Capital i and lowercase l can look near-identical in some fonts.\ne, c, o, and a can also be difficult to distinguish from each other in some fonts." }
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
