module Examples.Block exposing (Msg, State, example)

{-|

@docs Msg, State, example

-}

import Browser.Dom as Dom exposing (Element)
import Category exposing (Category(..))
import Code
import CommonControls
import Css
import Css.Global
import Debug.Control as Control exposing (Control)
import Debug.Control.Extra as ControlExtra
import Debug.Control.View as ControlView
import Dict exposing (Dict)
import Example exposing (Example)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Markdown
import Nri.Ui.Block.V5 as Block
import Nri.Ui.Button.V10 as Button
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Table.V7 as Table
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V1 as UiIcon
import Task


moduleName : String
moduleName =
    "Block"


version : Int
version =
    5


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , categories = [ Instructional ]
    , keyboardSupport = []
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ [ Block.view
                [ Block.plaintext "Dave"
                , Block.yellow
                ]
          , Block.view [ Block.plaintext " " ]
          , Block.view
                [ Block.plaintext "broke"
                , Block.cyan
                ]
          , Block.view [ Block.plaintext " his french fry so " ]
          , Block.view
                [ Block.plaintext "he"
                , Block.label "subject"
                , Block.yellow
                , Block.labelPosition (Just { totalHeight = 66, arrowHeight = 34, zIndex = 0, xOffset = 0 })
                ]
          , Block.view [ Block.plaintext " " ]
          , Block.view
                [ Block.plaintext "glued"
                , Block.label "verb"
                , Block.cyan
                , Block.labelPosition (Just { totalHeight = 34, arrowHeight = 8, zIndex = 0, xOffset = 0 })
                ]
          , Block.view [ Block.plaintext " it with ketchup." ]
          ]
            |> p [ css [ Fonts.baseFont, Css.fontSize (Css.px 12), Css.margin Css.zero ] ]
        ]
    , view =
        \ellieLinkConfig state ->
            let
                attributes =
                    Control.currentValue state.settings

                inParagraph =
                    p
                        [ css
                            [ Css.margin2 (Css.px 30) Css.zero
                            , Fonts.quizFont
                            , Css.fontSize (Css.px 30)
                            ]
                        ]

                offsets =
                    Block.getLabelPositions state.labelMeasurementsById
            in
            [ Heading.h2 [ Heading.plaintext "About" ]
            , Text.mediumBody
                [ Text.html
                    [ p []
                        [ text "You might also know the Block element as a “Display Element”. Learn more in "
                        , ClickableText.link "Display Elements and Scaffolding Container: additional things to know"
                            [ ClickableText.linkExternal "https://paper.dropbox.com/doc/Display-Elements-and-Scaffolding-Container-additional-things-to-know--BwRhBMKyXFFSWz~1mljN29bcAg-6vszpNDLoYIiMyg7Wv66s"
                            , ClickableText.rightIcon UiIcon.openInNewTab
                            , ClickableText.css [ Css.verticalAlign Css.baseline ]
                            ]
                        ]
                    ]
                ]
            , -- absolutely positioned elements that overflow in the x direction
              -- cause a horizontal scrollbar unless you explicitly hide overflowing x content
              Css.Global.global [ Css.Global.selector "body" [ Css.overflowX Css.hidden ] ]
            , ControlView.view
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
              , Block.view
                    (Block.labelPosition (Dict.get fruitId offsets)
                        :: List.map Tuple.second attributes
                    )
              , Block.view [ Block.plaintext " a lot!" ]
              ]
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
            , Button.button "Measure & render"
                [ Button.onClick GetBlockLabelMeasurements
                , Button.small
                , Button.secondary
                , Button.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Text.caption
                [ Text.plaintext "Click \"Measure & render\" to reposition the noninteractive examples' labels to avoid overlaps given the current viewport."
                , Text.css [ Css.marginBottom Spacing.verticalSpacerPx |> Css.important ]
                ]
            , p
                [ css
                    [ Fonts.quizFont
                    , Css.fontSize (Css.px 30)
                    , Css.marginTop (Css.px 60)
                    ]
                ]
                [ Block.view
                    [ Block.plaintext "Superman"
                    , Block.magenta
                    , Block.label "subject"
                    , Block.labelId subjectId
                    , Block.labelPosition (Dict.get subjectId offsets)
                    ]
                , Block.view [ Block.plaintext " " ]
                , Block.view []
                , Block.view [ Block.plaintext " " ]
                , Block.view
                    [ Block.plaintext "gifts"
                    , Block.label "direct object"
                    , Block.yellow
                    , Block.labelId directObjectId
                    , Block.labelPosition (Dict.get directObjectId offsets)
                    ]
                , Block.view [ Block.plaintext " " ]
                , Block.view
                    [ Block.label "preposition"
                    , Block.cyan
                    , Block.labelId prepositionId
                    , Block.labelPosition (Dict.get prepositionId offsets)
                    ]
                , Block.view <|
                    [ Block.content
                        [ Block.bold (List.concat [ Block.phrase " comic ", [ Block.italic (Block.phrase "book") ], Block.phrase " pages. " ])
                        ]
                    ]
                , Block.view
                    [ (List.concat >> Block.content)
                        [ Block.phrase "This is "
                        , [ Block.italic (Block.phrase "heroically") ]
                        , [ Block.bold (Block.phrase " generous ") ]
                        , [ Block.blank Block.ShortWordPhrase ]
                        , Block.phrase " each comic book costs about $5."
                        ]
                    , Block.label "Editor's note (can *also* include **markdown**!)"
                    , Block.brown
                    , Block.labelId editorsNoteId
                    , Block.labelPosition (Dict.get editorsNoteId offsets)
                    ]
                , Block.view
                    [ Block.plaintext " "
                    ]
                , Block.view
                    [ Block.content [ Block.italic (Block.phrase "Emphasized Italics.") ]
                    , Block.emphasize
                    ]
                ]
            , p
                [ css
                    [ Fonts.quizFont
                    , Css.fontSize (Css.px 30)
                    , Css.marginTop (Css.px 60)
                    , Css.whiteSpace Css.pre
                    ]
                ]
                [ Block.view
                    [ Block.plaintext "A"
                    , Block.magenta
                    , Block.label "this is an article. \"An\" is also an article."
                    , Block.labelId articleId
                    , Block.labelPosition (Dict.get articleId offsets)
                    ]
                , Block.view [ Block.plaintext " " ]
                , Block.view
                    [ Block.plaintext "tricky"
                    , Block.label "this is an adjective"
                    , Block.yellow
                    , Block.labelId trickyId
                    , Block.labelPosition (Dict.get trickyId offsets)
                    ]
                , Block.view [ Block.plaintext " " ]
                , Block.view
                    [ Block.plaintext "example"
                    , Block.label "the goal of which is to demonstrate horizontal repositioning"
                    , Block.cyan
                    , Block.labelId goalId
                    , Block.labelPosition (Dict.get goalId offsets)
                    ]
                , Block.view [ Block.plaintext ". Be sure to be " ]
                , Block.view
                    [ Block.plaintext "careful"
                    , Block.label "Shortish content..."
                    , Block.green
                    , Block.labelId shortId
                    , Block.labelPosition (Dict.get shortId offsets)
                    ]
                , Block.view
                    [ Block.plaintext "!"
                    , Block.label "It's important that the 'lowest' content is rendered on top of all other content."
                    , Block.blue
                    , Block.labelId longId
                    , Block.labelPosition (Dict.get longId offsets)
                    ]
                , Block.view
                    [ Block.plaintext "This is content"
                    ]
                , Block.view
                    [ Block.plaintext ", that should not break before this comma!"
                    , Block.label "Dont break"
                    , Block.labelId dontBreakId
                    , Block.labelPosition (Dict.get dontBreakId offsets)
                    ]
                ]
            , Table.view []
                [ Table.custom
                    { header = text "Blank Width"
                    , view = String.fromInt >> text
                    , width = Css.px 125
                    , cellStyles = always []
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Example"
                    , view = \characters -> Block.view [ Block.content [ Block.fullHeightBlank (Block.CharacterCount characters) ] ]
                    , width = Css.auto
                    , cellStyles = always [ Css.fontSize (Css.px 30) ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Code"
                    , view = \characters -> code [] [ text <| "Block.view \n  [ Block.content \n    [ Block.fullHeightBlank (Block.CharacterCount " ++ String.fromInt characters ++ ") \n    ] \n  ]" ]
                    , width = Css.px 500
                    , cellStyles =
                        always
                            [ Css.padding2 (Css.px 14) (Css.px 7)
                            , Css.verticalAlign Css.top
                            , Css.fontSize (Css.px 12)
                            , Css.whiteSpace Css.preWrap
                            ]
                    , sort = Nothing
                    }
                ]
                [ 1, 2, 3, 4, 5, 6, 7, 8 ]
            , Table.view []
                [ Table.custom
                    { header = text "Pattern name & description"
                    , view = .description >> Markdown.toHtml Nothing >> List.map fromUnstyled >> div []
                    , width = Css.px 125
                    , cellStyles = always [ Css.padding2 Css.zero (Css.px 7), Css.verticalAlign Css.top ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Example"
                    , view = .example
                    , width = Css.px 200
                    , cellStyles = always [ Css.textAlign Css.left ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Code"
                    , view = \{ pattern } -> code [] [ text pattern ]
                    , width = Css.px 50
                    , cellStyles =
                        always
                            [ Css.padding2 (Css.px 14) (Css.px 7)
                            , Css.verticalAlign Css.top
                            , Css.fontSize (Css.px 12)
                            , Css.whiteSpace Css.preWrap
                            ]
                    , sort = Nothing
                    }
                ]
                [ { pattern = Code.fromModule moduleName "view " ++ Code.list [ Code.fromModule moduleName "emphasize", "…" ]
                  , description =
                        """**Emphasis block**

- Uses the Highlighter component to mark content as emphasized
- Help students focus in on a specific part of the content
- Often a phrase or clause
"""
                  , example =
                        inParagraph
                            [ Block.view [ Block.plaintext "The Crossover", Block.emphasize ]
                            , Block.view [ Block.plaintext " is Thor’s favorite book." ]
                            ]
                  }
                , { pattern =
                        Code.fromModule moduleName "view"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "label " ++ Code.string "[label text]"
                                , Code.fromModule moduleName "plaintext " ++ Code.string "[text]"
                                , "…"
                                ]
                                1
                  , description = "**Label block**\n\nHelp students understand the function different words and phrases are playing in a sentence"
                  , example =
                        div []
                            [ inParagraph
                                [ Block.view [ Block.plaintext "Taylor Swift bought " ]
                                , Block.view
                                    [ Block.plaintext "new"
                                    , Block.label "age"
                                    , Block.labelId ageId
                                    , Block.labelPosition (Dict.get ageId offsets)
                                    , Block.yellow
                                    ]
                                , Block.view [ Block.plaintext " " ]
                                , Block.view
                                    [ Block.plaintext "bowling"
                                    , Block.label "purpose"
                                    , Block.labelId purposeId
                                    , Block.labelPosition (Dict.get purposeId offsets)
                                    , Block.cyan
                                    ]
                                , Block.view [ Block.plaintext " " ]
                                , Block.view
                                    [ Block.plaintext "yellow"
                                    , Block.label "color"
                                    , Block.labelId colorId
                                    , Block.labelPosition (Dict.get colorId offsets)
                                    , Block.magenta
                                    ]
                                , Block.view [ Block.plaintext " shoes." ]
                                ]
                            ]
                  }
                , { pattern = Code.fromModule moduleName "view []"
                  , description = "**Blank block**\n\nRepresents a blank in the sentence."
                  , example =
                        inParagraph
                            [ Block.view [ Block.plaintext "I am a seed with " ]
                            , Block.view []
                            , Block.view [ Block.plaintext " being used." ]
                            ]
                  }
                , { pattern =
                        Code.fromModule moduleName "view"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "content"
                                    ++ Code.listMultiline
                                        [ "…"
                                        , Code.fromModule moduleName "blankWithId "
                                            ++ Code.string "[id]"
                                            ++ " "
                                            ++ Code.fromModule moduleName "SingleCharacter"
                                        , "…"
                                        ]
                                        2
                                ]
                                1
                  , description = "**Blank block with id and SingleCharacter length**\n\n"
                  , example =
                        inParagraph
                            [ Block.view [ Block.plaintext "I am a seed with " ]
                            , Block.view
                                [ Block.content
                                    [ Block.blankWithId "blank-with-id-single-character-example" Block.SingleCharacter
                                    ]
                                ]
                            , Block.view [ Block.plaintext " being used." ]
                            ]
                  }
                , { pattern =
                        Code.fromModule moduleName "view"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "content"
                                    ++ Code.listMultiline
                                        [ "…"
                                        , Code.fromModule moduleName "blankWithId "
                                            ++ Code.string "[id]"
                                            ++ " "
                                            ++ Code.fromModule moduleName "ShortWordPhrase"
                                        , "…"
                                        ]
                                        2
                                ]
                                1
                  , description = "**Blank block with id and ShortWordPhrase length**\n\n"
                  , example =
                        inParagraph
                            [ Block.view [ Block.plaintext "I am a seed with " ]
                            , Block.view
                                [ Block.content
                                    [ Block.blankWithId "blank-with-id-short-word-example" Block.ShortWordPhrase
                                    ]
                                ]
                            , Block.view [ Block.plaintext " being used." ]
                            ]
                  }
                , { pattern =
                        Code.fromModule moduleName "view"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "content"
                                    ++ Code.listMultiline
                                        [ "…"
                                        , Code.fromModule moduleName "blankWithId "
                                            ++ Code.string "[id]"
                                            ++ " "
                                            ++ Code.fromModule moduleName "LongWordPhrase"
                                        , "…"
                                        ]
                                        2
                                ]
                                1
                  , description = "**Blank block with id and LongWordPhrase length**\n\n"
                  , example =
                        inParagraph
                            [ Block.view [ Block.plaintext "I am a seed with " ]
                            , Block.view
                                [ Block.content
                                    [ Block.blankWithId "blank-with-id-long-word-example" Block.LongWordPhrase
                                    ]
                                ]
                            , Block.view [ Block.plaintext " being used." ]
                            ]
                  }
                , { pattern =
                        Code.fromModule moduleName "view"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "content"
                                    ++ Code.listMultiline
                                        [ "…"
                                        , Code.fromModule moduleName "blankWithId "
                                            ++ Code.string "[id]"
                                            ++ " "
                                            ++ Code.withParens (Code.fromModule moduleName "CharacterCount 8")
                                        , "…"
                                        ]
                                        2
                                ]
                                1
                  , description = "**Blank block with id and CharacterCount 8 length**\n\n"
                  , example =
                        inParagraph
                            [ Block.view [ Block.plaintext "I am a seed with " ]
                            , Block.view
                                [ Block.content
                                    [ Block.blankWithId "blank-with-id-character-count-example" (Block.CharacterCount 8)
                                    ]
                                ]
                            , Block.view [ Block.plaintext " being used." ]
                            ]
                  }
                , { pattern =
                        Code.fromModule moduleName "view"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "content"
                                    ++ Code.listMultiline
                                        [ "…"
                                        , Code.fromModule moduleName "fullHeightBlank "
                                            ++ Code.fromModule moduleName "SingleCharacter"
                                        , "…"
                                        ]
                                        2
                                ]
                                1
                  , description = "**Full height blank block and SingleCharacter length**\n\n"
                  , example =
                        inParagraph
                            [ Block.view [ Block.plaintext "I am a seed with " ]
                            , Block.view
                                [ Block.content
                                    [ Block.fullHeightBlank Block.SingleCharacter
                                    ]
                                ]
                            , Block.view [ Block.plaintext " being used." ]
                            ]
                  }
                , { pattern =
                        Code.fromModule moduleName "view"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "content"
                                    ++ Code.listMultiline
                                        [ "…"
                                        , Code.fromModule moduleName "fullHeightBlank "
                                            ++ Code.fromModule moduleName "ShortWordPhrase"
                                        , "…"
                                        ]
                                        2
                                ]
                                1
                  , description = "**Full height blank block and ShortWordPhrase length**\n\n"
                  , example =
                        inParagraph
                            [ Block.view [ Block.plaintext "I am a seed with " ]
                            , Block.view
                                [ Block.content
                                    [ Block.fullHeightBlank Block.ShortWordPhrase
                                    ]
                                ]
                            , Block.view [ Block.plaintext " being used." ]
                            ]
                  }
                , { pattern =
                        Code.fromModule moduleName "view"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "content"
                                    ++ Code.listMultiline
                                        [ "…"
                                        , Code.fromModule moduleName "fullHeightBlank "
                                            ++ Code.fromModule moduleName "LongWordPhrase"
                                        , "…"
                                        ]
                                        2
                                ]
                                1
                  , description = "**Full height blank block and LongWordPhrase length**\n\n"
                  , example =
                        inParagraph
                            [ Block.view [ Block.plaintext "I am a seed with " ]
                            , Block.view
                                [ Block.content
                                    [ Block.fullHeightBlank Block.LongWordPhrase
                                    ]
                                ]
                            , Block.view [ Block.plaintext " being used." ]
                            ]
                  }
                , { pattern =
                        Code.fromModule moduleName "view"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "content"
                                    ++ Code.listMultiline
                                        [ "…"
                                        , Code.fromModule moduleName "fullHeightBlank "
                                            ++ Code.withParens (Code.fromModule moduleName "CharacterCount 8")
                                        , "…"
                                        ]
                                        2
                                ]
                                1
                  , description = "**Full height blank block and CharacterCount 8 length**\n\n"
                  , example =
                        inParagraph
                            [ Block.view [ Block.plaintext "I am a seed with " ]
                            , Block.view
                                [ Block.content
                                    [ Block.fullHeightBlank (Block.CharacterCount 8)
                                    ]
                                ]
                            , Block.view [ Block.plaintext " being used." ]
                            ]
                  }
                , { pattern =
                        Code.fromModule moduleName "view "
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "label " ++ Code.string "[label text]"
                                , Code.fromModule moduleName "purple"
                                ]
                                1
                  , description = "**Labeled blank block**\n\nA labelled blank in the sentence"
                  , example =
                        inParagraph
                            [ Block.view [ Block.plaintext "If a volcano is extinct, " ]
                            , Block.view
                                [ Block.label "pronoun"
                                , Block.purple
                                , Block.labelId pronounId
                                , Block.labelPosition (Dict.get pronounId offsets)
                                ]
                            , Block.view [ Block.plaintext " will never erupt again." ]
                            ]
                  }
                , { pattern =
                        Code.fromModule moduleName "view "
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "emphasize"
                                , Code.fromModule moduleName "content "
                                    ++ Code.listMultiline
                                        [ "…"
                                        , Code.fromModule moduleName "blank "
                                            ++ Code.fromModule moduleName "ShortWordPhrase"
                                        , "…"
                                        ]
                                        2
                                ]
                                1
                  , description = "**Blanks with emphasis block**\n\nHelp students focus in on a phrase that includes a blank"
                  , example =
                        inParagraph
                            [ Block.view [ Block.plaintext "This is an " ]
                            , Block.view
                                [ Block.emphasize
                                , (List.concat >> Block.content)
                                    [ Block.phrase "emphasized subsegement "
                                    , [ Block.blank Block.ShortWordPhrase ]
                                    , Block.phrase " emphasized"
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
    , labelMeasurementsById :
        Dict
            String
            { label : Element
            , labelContent : Element
            }
    }


{-| -}
init : State
init =
    { settings = initControl
    , labelMeasurementsById = Dict.empty
    }


type alias Settings =
    List ( String, Block.Attribute Msg )


initControl : Control Settings
initControl =
    ControlExtra.list
        |> ControlExtra.optionalListItem "content" controlContent
        |> ControlExtra.optionalBoolListItem "emphasize" ( Code.fromModule moduleName "emphasize", Block.emphasize )
        |> ControlExtra.optionalListItem "label"
            (CommonControls.string ( Code.fromModule moduleName "label", Block.label ) "Fruit")
        |> ControlExtra.optionalListItem "labelPosition"
            (Control.map
                (\( code, v ) ->
                    ( Code.fromModule moduleName "labelPosition (Just" ++ code ++ ")"
                    , Block.labelPosition (Just v)
                    )
                )
                (Control.record
                    (\a b c ->
                        ( Code.record
                            [ ( "arrowHeight", String.fromFloat a )
                            , ( "totalHeight", String.fromFloat b )
                            , ( "zIndex", "0" )
                            , ( "xOffset", String.fromFloat c )
                            ]
                        , { arrowHeight = a, totalHeight = b, zIndex = 0, xOffset = c }
                        )
                    )
                    |> Control.field "arrowHeight" (ControlExtra.float 40)
                    |> Control.field "totalHeight" (ControlExtra.float 80)
                    |> Control.field "xOffset" (ControlExtra.float 0)
                )
            )
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
        |> ControlExtra.optionalListItem "labelId"
            (CommonControls.string ( Code.fromModule moduleName "labelId", Block.labelId ) fruitId)
        |> ControlExtra.optionalListItem "labelState"
            (Control.choice
                [ ( "Visible"
                  , Control.value
                        ( Code.fromModule moduleName "labelState "
                            ++ Code.fromModule moduleName "Visible "
                        , Block.labelState Block.Visible
                        )
                  )
                , ( "FadeOut"
                  , Control.value
                        ( Code.fromModule moduleName "labelState "
                            ++ Code.fromModule moduleName "FadeOut "
                        , Block.labelState Block.FadeOut
                        )
                  )
                ]
            )


controlContent : Control ( String, Block.Attribute msg )
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
                    ++ Code.withParens
                        ((Code.fromModule moduleName "string " ++ Code.string "to think about ")
                            ++ (" ++ " ++ Code.fromModule moduleName "blank")
                            ++ (" :: " ++ Code.fromModule moduleName "string " ++ Code.string " and so forth")
                        )
                , Block.content
                    (Block.phrase "to think about "
                        ++ Block.blank Block.ShortWordPhrase
                        :: Block.phrase " and so forth"
                    )
                )
          )
        , ( "single character blank"
          , Control.value
                ( Code.fromModule moduleName "content "
                    ++ Code.withParens
                        (Code.fromModule moduleName "blank "
                            ++ Code.fromModule moduleName "SingleCharacter"
                        )
                , Block.content [ Block.blank Block.SingleCharacter ]
                )
          )
        , ( "short word phrase blank"
          , Control.value
                ( Code.fromModule moduleName "content "
                    ++ Code.withParens
                        (Code.fromModule moduleName "blank "
                            ++ Code.fromModule moduleName "ShortWordPhrase"
                        )
                , Block.content [ Block.blank Block.ShortWordPhrase ]
                )
          )
        , ( "long word phrase blank"
          , Control.value
                ( Code.fromModule moduleName "content "
                    ++ Code.withParens
                        (Code.fromModule moduleName "blank "
                            ++ Code.fromModule moduleName "LongWordPhrase"
                        )
                , Block.content [ Block.blank Block.LongWordPhrase ]
                )
          )
        , ( "full height single character blank"
          , Control.value
                ( Code.fromModule moduleName "content "
                    ++ Code.withParens
                        (Code.fromModule moduleName "fullHeightBlank "
                            ++ Code.fromModule moduleName "SingleCharacter"
                        )
                , Block.content [ Block.fullHeightBlank Block.SingleCharacter ]
                )
          )
        , ( "full height short word phrase blank"
          , Control.value
                ( Code.fromModule moduleName "content "
                    ++ Code.withParens
                        (Code.fromModule moduleName "fullHeightBlank "
                            ++ Code.fromModule moduleName "ShortWordPhrase"
                        )
                , Block.content [ Block.fullHeightBlank Block.ShortWordPhrase ]
                )
          )
        , ( "full height long word phrase blank"
          , Control.value
                ( Code.fromModule moduleName "content "
                    ++ Code.withParens
                        (Code.fromModule moduleName "fullHeightBlank "
                            ++ Code.fromModule moduleName "LongWordPhrase"
                        )
                , Block.content [ Block.fullHeightBlank Block.LongWordPhrase ]
                )
          )
        ]


ageId : String
ageId =
    "age-label-id"


colorId : String
colorId =
    "color-label-id"


fruitId : String
fruitId =
    "fruit-block"


purposeId : String
purposeId =
    "purpose-label-id"


subjectId : String
subjectId =
    "subject-label-id"


directObjectId : String
directObjectId =
    "direct-object-label-id"


prepositionId : String
prepositionId =
    "preposition-label-id"


editorsNoteId : String
editorsNoteId =
    "editors-note-label-id"


pronounId : String
pronounId =
    "pronoun-label-id"


articleId : String
articleId =
    "article-label-id"


trickyId : String
trickyId =
    "tricky-label-id"


goalId : String
goalId =
    "goal-label-id"


shortId : String
shortId =
    "short-label-id"


longId : String
longId =
    "long-label-id"


dontBreakId : String
dontBreakId =
    "do-not-break-label-id"


blocksWithLabelsIds : List String
blocksWithLabelsIds =
    [ ageId
    , colorId
    , purposeId
    , subjectId
    , directObjectId
    , prepositionId
    , editorsNoteId
    , pronounId
    , articleId
    , trickyId
    , goalId
    , shortId
    , longId
    , dontBreakId
    ]


{-| -}
type Msg
    = UpdateSettings (Control Settings)
    | GetBlockLabelMeasurements
    | GotBlockLabelMeasurements
        String
        (Result
            Dom.Error
            { label : Element
            , labelContent : Element
            }
        )


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg state =
    case msg of
        UpdateSettings newControl ->
            ( { state | settings = newControl }
            , measure fruitId
            )

        GetBlockLabelMeasurements ->
            ( state
            , Cmd.batch (List.map measure blocksWithLabelsIds)
            )

        GotBlockLabelMeasurements id (Ok measurement) ->
            ( { state | labelMeasurementsById = Dict.insert id measurement state.labelMeasurementsById }
            , Cmd.none
            )

        GotBlockLabelMeasurements _ (Err _) ->
            ( state
            , -- in a real application, log an error
              Cmd.none
            )


measure : String -> Cmd Msg
measure labelId =
    Task.map2 (\label labelContent -> { label = label, labelContent = labelContent })
        (Dom.getElement labelId)
        (Dom.getElement (Block.labelContentId labelId))
        |> Task.attempt (GotBlockLabelMeasurements labelId)
