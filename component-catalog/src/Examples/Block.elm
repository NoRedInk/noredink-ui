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
import Nri.Ui.Block.V6 as Block
import Nri.Ui.Button.V10 as Button
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Table.V7 as Table
import Nri.Ui.Text.V6 as Text
import Task


moduleName : String
moduleName =
    "Block"


version : Int
version =
    6


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
    , about =
        [ text "You might also know the Block element as a “Display Element”. Learn more in "
        , ClickableText.link "Display Elements and Scaffolding Container: additional things to know"
            [ ClickableText.linkExternal "https://paper.dropbox.com/doc/Display-Elements-and-Scaffolding-Container-additional-things-to-know--BwRhBMKyXFFSWz~1mljN29bcAg-6vszpNDLoYIiMyg7Wv66s"
            ]
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
            [ -- absolutely positioned elements that overflow in the x direction
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
                [ Heading.plaintext "Customizable example"
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
                    , Block.emphasize
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
                        , [ Block.blank { widthInChars = 8 } ]
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
                , { pattern =
                        Code.fromModule moduleName "view "
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "label " ++ Code.string "[label text]"
                                , Code.fromModule moduleName "purple"
                                ]
                                1
                  , description =
                        """**Labeled blank block**

A labelled blank in the sentence.

"""
                            ++ "Please see the \""
                            ++ blankWidthGuidanceSectionName
                            ++ "\" table to learn more about using Blanks."
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
                                            ++ Code.record [ ( "widthInChars", "8" ) ]
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
                                    , [ Block.blank { widthInChars = 8 } ]
                                    , Block.phrase " emphasized"
                                    ]
                                ]
                            , Block.view
                                [ Block.plaintext " in a seed."
                                ]
                            ]
                  }
                , { pattern =
                        Code.fromModule moduleName "view "
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "emphasize"
                                , Code.fromModule moduleName "content "
                                    ++ Code.listMultiline
                                        [ "…"
                                        , Code.fromModule moduleName "bold ("
                                            ++ (Code.fromModule moduleName "phrase " ++ "\"[bold text]\"")
                                            ++ ")"
                                        , Code.fromModule moduleName "italic ("
                                            ++ (Code.fromModule moduleName "phrase " ++ "\"[italic text]\"")
                                            ++ ")"
                                        , "…"
                                        ]
                                        2
                                ]
                                1
                  , description = "**Bold & Italic markdown**\n\nItalicize titles and highlight specific words inside of an emphasis block."
                  , example =
                        inParagraph
                            [ Block.view [ Block.plaintext "The " ]
                            , Block.view
                                [ Block.emphasize
                                , (List.concat >> Block.content)
                                    [ [ Block.bold (Block.phrase "quick ") ]
                                    , [ Block.italic (Block.phrase "brown ") ]
                                    , Block.phrase "fox"
                                    ]
                                ]
                            , Block.view [ Block.plaintext " jumped over the lazy dog." ]
                            ]
                  }
                ]
            , Heading.h2
                [ Heading.plaintext blankWidthGuidanceSectionName
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , Text.smallBody [ Text.markdown "The accessible name of all blanks, regardless of character width used, is \"blank.\"" ]
            , Text.smallBody [ Text.markdown "If we're looking for a specific length of content to put in the blank, that _must_ be communicated elsewhere on the page to provide an equitable experience." ]
            , Text.smallBody [ Text.markdown "The `widthInChars` parameter uses the number of characters expected in the blank to calculate a rough monospace-based width that visually looks like it _could_ match." ]
            , Table.view
                []
                [ Table.custom
                    { header = text "Pattern"
                    , view =
                        \{ code } ->
                            Html.Styled.code [] [ text code ]
                    , width = Css.px 400
                    , cellStyles =
                        always
                            [ Css.padding2 (Css.px 14) (Css.px 7)
                            , Css.verticalAlign Css.top
                            , Css.fontSize (Css.px 12)
                            , Css.whiteSpace Css.preWrap
                            ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Description"
                    , view =
                        .description
                            >> Markdown.toHtml Nothing
                            >> List.map fromUnstyled
                            >> div []
                    , width = Css.px 300
                    , cellStyles = always [ Css.padding2 Css.zero (Css.px 7), Css.verticalAlign Css.top ]
                    , sort = Nothing
                    }
                , Table.custom
                    { header = text "Example"
                    , view =
                        \{ textExample, blankExample } ->
                            div []
                                [ div [] [ Block.view (Block.emphasize :: textExample) ]
                                , div [] [ Block.view blankExample ]
                                ]
                    , width = Css.px 300
                    , cellStyles = always [ Css.padding2 (Css.px 4) (Css.px 7), Css.verticalAlign Css.top ]
                    , sort = Nothing
                    }
                ]
                [ { code = Code.fromModule moduleName "view []"
                  , description = "Default view of a blank, without customization."
                  , textExample = [ Block.plaintext "" ]
                  , blankExample = []
                  }
                , { code =
                        Code.fromModule moduleName "view"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "contents"
                                    ++ Code.listMultiline
                                        [ Code.fromModule moduleName "blank " ++ Code.record [ ( "widthInChars", "1" ) ]
                                        ]
                                        2
                                ]
                                1
                  , description = "A single character.  Typically used to represent punctuation."
                  , textExample = [ Block.plaintext "," ]
                  , blankExample = [ Block.content [ Block.blank { widthInChars = 1 } ] ]
                  }
                , { code =
                        Code.fromModule moduleName "view"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "contents"
                                    ++ Code.listMultiline
                                        [ Code.fromModule moduleName "blank " ++ Code.record [ ( "widthInChars", "8" ) ]
                                        ]
                                        2
                                ]
                                1
                  , description = "A short word or phrase.  "
                  , textExample = [ Block.plaintext "a phrase" ]
                  , blankExample = [ Block.content [ Block.blank { widthInChars = 8 } ] ]
                  }
                , { code =
                        Code.fromModule moduleName "view"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "contents"
                                    ++ Code.listMultiline
                                        [ Code.fromModule moduleName "blank " ++ Code.record [ ( "widthInChars", "16" ) ]
                                        ]
                                        2
                                ]
                                1
                  , description = "A long word or phrase."
                  , textExample = [ Block.plaintext "multifariousness" ]
                  , blankExample = [ Block.content [ Block.blank { widthInChars = 16 } ] ]
                  }
                ]
            ]
    }


blankWidthGuidanceSectionName : String
blankWidthGuidanceSectionName =
    "Blank width guidance"


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
        |> ControlExtra.optionalListItemDefaultChecked "content" controlContent
        |> ControlExtra.optionalBoolListItemDefaultChecked "emphasize" ( Code.fromModule moduleName "emphasize", Block.emphasize )
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
                        ((Code.fromModule moduleName "italic (" ++ Code.fromModule moduleName "phrase " ++ Code.string "to think about" ++ ")")
                            ++ (" :: " ++ Code.fromModule moduleName "blank")
                            ++ (" :: " ++ Code.fromModule moduleName "phrase " ++ Code.string " and so forth")
                        )
                , Block.content
                    (Block.italic (Block.phrase "to think about ")
                        :: Block.blank { widthInChars = 8 }
                        :: Block.phrase " and so forth"
                    )
                )
          )
        , blankType ( "blank", Block.blank )
        ]


blankType : ( String, { widthInChars : Int } -> Block.Content msg ) -> ( String, Control ( String, Block.Attribute msg ) )
blankType ( typeStr, blank ) =
    ( typeStr
    , Control.map
        (\( widthString, width ) ->
            ( Code.fromModule moduleName "content "
                ++ Code.withParens
                    (Code.fromModule moduleName typeStr
                        ++ " "
                        ++ widthString
                    )
            , Block.content [ blank { widthInChars = width } ]
            )
        )
        controlBlankWidth
    )


controlBlankWidth : Control ( String, Int )
controlBlankWidth =
    Control.record
        (\widthInChars ->
            ( Code.record
                [ ( "widthInChars", String.fromInt widthInChars )
                ]
            , widthInChars
            )
        )
        |> Control.field "widthInChars" (ControlExtra.int 8)


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
