module Examples.Block exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Markdown
import Nri.Ui.Block.V1 as Block
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Table.V6 as Table


moduleName : String
moduleName =
    "Block"


version : Int
version =
    1


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Interactions ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ [ Block.view
                [ Block.plaintext "Dave"
                , Block.label "subject"
                , Block.yellow
                ]
          , Block.view [ Block.plaintext " " ]
          , Block.view
                [ Block.plaintext "broke"
                , Block.label "verb"
                , Block.cyan
                ]
          , Block.view [ Block.plaintext " his french fry so " ]
          , Block.view [ Block.plaintext "he", Block.yellow ]
          , Block.view [ Block.plaintext " " ]
          , Block.view [ Block.plaintext "glued", Block.cyan ]
          , Block.view [ Block.plaintext " it with ketchup." ]
          ]
            |> List.concat
            |> p
                [ css
                    [ Fonts.baseFont
                    , Css.fontSize (Css.px 12)
                    , Css.marginTop (Css.px 40)
                    , Css.lineHeight (Css.num 2.5)
                    ]
                ]
        ]
    , view =
        \ellieLinkConfig state ->
            let
                attributes =
                    Control.currentValue state.settings
            in
            [ ControlView.view
                { ellieLinkConfig = ellieLinkConfig
                , name = moduleName
                , version = version
                , update = UpdateSettings
                , settings = state.settings
                , mainType = Nothing
                , extraCode = []
                , renderExample = Code.unstyledView
                , toExampleCode =
                    \_ ->
                        [ { sectionName = "view"
                          , code =
                                Code.fromModule moduleName "view "
                                    ++ Code.list (List.map Tuple.first attributes)
                          }
                        ]
                }
            , Heading.h2
                [ Heading.plaintext "Interactive example"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , [ Block.view [ Block.plaintext "I like " ]
              , Block.view (List.map Tuple.second attributes)
              , Block.view [ Block.plaintext " a lot!" ]
              ]
                |> List.concat
                |> p
                    [ css
                        [ Fonts.quizFont
                        , Css.fontSize (Css.px 30)
                        , Css.textAlign Css.center
                        ]
                    ]
            , Heading.h2
                [ Heading.plaintext "Non-interactive examples"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Table.view
                [ Table.custom
                    { header = text "Pattern"
                    , view = \{ pattern } -> code [] [ text pattern ]
                    , width = Css.px 50
                    , cellStyles = always [ Css.padding2 (Css.px 14) (Css.px 7), Css.verticalAlign Css.top, Css.fontSize (Css.px 12) ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "About"
                    , view = .description >> Markdown.toHtml Nothing >> List.map fromUnstyled >> span []
                    , width = Css.px 125
                    , cellStyles = always [ Css.padding2 Css.zero (Css.px 7), Css.verticalAlign Css.top ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Example"
                    , view = .example >> List.concat >> p [ css [ Css.margin2 (Css.px 30) Css.zero ] ]
                    , width = Css.px 200
                    , cellStyles = always [ Css.textAlign Css.center ]
                    , sort = Nothing
                    }
                ]
                [ { pattern = "Code.view []"
                  , description = "Represents a blank in the sentence. Expected to be used in Cycling interface scaffolding."
                  , example =
                        [ Block.view [ Block.plaintext "I am a seed with " ]
                        , Block.view []
                        , Block.view [ Block.plaintext " being used." ]
                        ]
                  }
                , { pattern = "Code.view [ Code.label \"[label text]\" ]"
                  , description = "A labelled blank in the sentence"
                  , example =
                        [ Block.view [ Block.plaintext "If a volcano is extinct, " ]
                        , Block.view [ Block.label "pronoun" ]
                        , Block.view [ Block.plaintext " will never erupt again." ]
                        ]
                  }
                , { pattern = "Code.view [ Code.label \"[label text]\", Code.plaintext \"[text]\", … ]"
                  , description = "Help students understand the function different words and phrases are playing in a sentence"
                  , example =
                        [ Block.view [ Block.plaintext "Taylor Swift bought " ]
                        , Block.view [ Block.plaintext "new", Block.label "age", Block.yellow ]
                        , Block.view [ Block.plaintext " " ]
                        , Block.view [ Block.plaintext "bowling", Block.label "purpose", Block.cyan ]
                        , Block.view [ Block.plaintext " " ]
                        , Block.view [ Block.plaintext "yellow", Block.label "color", Block.magenta ]
                        , Block.view [ Block.plaintext " shoes." ]
                        ]
                  }
                , { pattern = "Code.view [ Code.emphasize, … ]"
                  , description =
                        """
- Uses the Highlighter component to mark content as emphasized
- Help students focus in on a specific part of the content
- Often a phrase or clause
"""
                  , example =
                        [ Block.view [ Block.plaintext "The Crossover", Block.emphasize ]
                        , Block.view [ Block.plaintext " is Thor’s favorite book." ]
                        ]
                  }
                , { pattern = "Code.view [ Code.emphasize, Code.content [ … ] ]"
                  , description = "Help students focus in on a phrase that includes a blank"
                  , example =
                        [ Block.view [ Block.plaintext "This is an " ]
                        , Block.view
                            [ Block.emphasize
                            , Block.content
                                [ Block.string "emphasized subsegement "
                                , Block.blank
                                , Block.string " emphasized"
                                ]
                            ]
                        , Block.view
                            [ Block.plaintext " in a seed."
                            ]
                        ]
                  }
                ]
            ]
    }


{-| -}
type alias State =
    { settings : Control Settings
    }


{-| -}
init : State
init =
    { settings = initControl
    }


type alias Settings =
    List ( String, Block.Attribute )


initControl : Control Settings
initControl =
    ControlExtra.list
        |> ControlExtra.optionalListItem "content" controlContent
        |> ControlExtra.optionalBoolListItem "emphasize" ( Code.fromModule moduleName "emphasize", Block.emphasize )
        |> ControlExtra.optionalListItem "label"
            (CommonControls.string ( Code.fromModule moduleName "label", Block.label ) "Fruit")
        |> ControlExtra.optionalListItem "theme"
            (CommonControls.choice moduleName
                [ ( "yellow", Block.yellow )
                , ( "cyan", Block.cyan )
                , ( "magenta", Block.magenta )
                , ( "green", Block.green )
                , ( "blue", Block.blue )
                , ( "purple", Block.purple )
                , ( "brown", Block.brown )
                ]
            )
        |> ControlExtra.optionalListItem "class"
            (CommonControls.string ( "class", Block.class ) "kiwis-are-good")


controlContent : Control ( String, Block.Attribute )
controlContent =
    Control.choice
        [ ( "plaintext"
          , CommonControls.string
                ( Code.fromModule moduleName "plaintext"
                , Block.plaintext
                )
                "bananas"
          )
        , ( "with mixed content"
          , Control.value
                ( Code.fromModule moduleName "content "
                    ++ Code.listMultiline
                        [ Code.fromModule moduleName "string " ++ Code.string "to think about "
                        , Code.fromModule moduleName "blank"
                        , Code.fromModule moduleName "string " ++ Code.string " and so forth"
                        ]
                        2
                , Block.content
                    [ Block.string "to think about "
                    , Block.blank
                    , Block.string " and so forth"
                    ]
                )
          )
        ]


{-| -}
type Msg
    = UpdateSettings (Control Settings)


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateSettings newControl ->
            ( { state | settings = newControl }
            , Cmd.none
            )
