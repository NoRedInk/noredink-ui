module Examples.Accordion exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import Accessibility.Styled as Html exposing (..)
import Browser.Dom as Dom
import Category exposing (Category(..))
import Code
import CommonControls
import Css exposing (..)
import Css.Global
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Guidance
import Html.Styled.Attributes as Attributes exposing (css, src)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Accordion.V4 as Accordion exposing (AccordionEntry(..))
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Colors.Extra as ColorsExtra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V2 as UiIcon
import Set exposing (Set)
import Task


moduleName : String
moduleName =
    "Accordion"


version : Int
version =
    4


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , init = ( init, Cmd.none )
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ -- faking a mini version of the Accordion component to give Component Catalog users a sense of what the
          -- component might look like
          Html.div []
            [ Html.div [ css [ Css.displayFlex, Css.alignItems Css.center ] ]
                [ Accordion.defaultCaret False
                , Text.smallBody [ Text.plaintext "Closed" ]
                ]
            , Html.div [ css [ Css.displayFlex, Css.alignItems Css.center ] ]
                [ Accordion.defaultCaret True
                , Text.smallBody [ Text.plaintext "Open" ]
                ]
            , Text.caption [ Text.plaintext "Accordion content." ]
            ]
        ]
    , about =
        [ ul [ css [ paddingLeft (px 16), margin zero ] ]
            [ li [] [ Text.smallBody [ Text.plaintext "The Accordion component is designed to be used when you have either a list or a tree of expandables. It may also be used when there's only one expandable. " ] ]
            , li []
                [ Text.smallBody
                    [ Text.html
                        [ text "Devs should watch the entirety of "
                        , ClickableText.link "Tessa's Accordion demo"
                            [ ClickableText.linkExternal "https://noredink.zoom.us/rec/play/kLjOvS0PX5y-YYas6VmtUf5eEb1ViqNKKB-01gCELXG5tMjINEdop6dXrmfCyfC1A3Xj9kTUK8eIDe0.LO5NQR0X3udwz13x?canPlayFromShare=true&from=share_recording_detail&startTime=1681398154000&componentName=rec-play&originRequestUrl=https%3A%2F%2Fnoredink.zoom.us%2Frec%2Fshare%2F6R2Tk0FkzAYJ-44Q4Qs2Dx2RPR1GCXOCcaQsEai6vbtkO8oo9Ke8-_goIVwPDn9I.VVXdtb2PlpuTEqGs%3FstartTime%3D1681398154000"
                            , ClickableText.appearsInline
                            ]
                        , text " before using. Discussion of how to attach styles correctly begins at 5:10."
                        ]
                    ]
                ]
            , li [] [ Guidance.communicateState moduleName ]
            ]
        ]
    , view = view
    , categories = [ Layout ]
    , keyboardSupport =
        [ { keys = [ Arrow KeyboardSupport.Up ]
          , result = "Moves the focus to the previous accordion header button (wraps focus to the last header button)"
          }
        , { keys = [ Arrow KeyboardSupport.Down ]
          , result = "Moves the focus to the next accordion header button (wraps focus to the first header button)"
          }
        , { keys = [ Arrow KeyboardSupport.Right ]
          , result = "Moves the focus to the first accordion header button in a nested list of accordions"
          }
        , { keys = [ Arrow KeyboardSupport.Left ]
          , result = "Moves the focus to the parent accordion header button from a a nested accordion"
          }
        ]
    }


{-| -}
view : EllieLink.Config -> State -> List (Html Msg)
view ellieLinkConfig model =
    let
        settings_ =
            Control.currentValue model.settings
    in
    [ ControlView.view
        { ellieLinkConfig = ellieLinkConfig
        , name = moduleName
        , version = version
        , update = UpdateControls
        , settings = model.settings
        , mainType = Just "RootHtml.Html String"
        , extraCode = [ "import Nri.Ui.Svg.V1 as Svg" ]
        , renderExample = Code.unstyledView
        , toExampleCode =
            \settings ->
                [ { sectionName = "Partial example"
                  , code =
                        "div []"
                            ++ Code.listMultiline
                                [ Code.fromModule moduleName "view"
                                    ++ Code.recordMultiline
                                        [ ( "entries"
                                          , Code.listMultiline
                                                [ Code.fromModule moduleName "AccordionEntry"
                                                    ++ Code.recordMultiline
                                                        [ ( "caret", Tuple.first settings.icon )
                                                        , ( "content", Code.anonymousFunction "()" (Tuple.first settings.content) )
                                                        , ( "entryClass", Code.string "customizable-example" )
                                                        , ( "expansionDirection", Tuple.first settings.expansionDirection )
                                                        , ( "headerContent", Tuple.first settings.headerContent )
                                                        , ( "headerId", Code.string "customizable-example-header" )
                                                        , ( "headerLevel", Code.fromModule moduleName "H3" )
                                                        , ( "isExpanded", "True" )
                                                        , ( "toggle", "Nothing" )
                                                        ]
                                                        4
                                                    ++ Code.listMultiline [] 4
                                                ]
                                                3
                                          )
                                        , ( "focus", "identity -- When using Accordion, be sure to wire up Focus management correctly!" )
                                        ]
                                        2
                                , Code.fromModule moduleName "styleAccordion"
                                    ++ Code.recordMultiline
                                        [ ( "entryStyles", Code.list [] )
                                        , ( "entryExpandedStyles", Code.list [] )
                                        , ( "entryClosedStyles", Code.list [] )
                                        , ( "headerStyles", Code.list [] )
                                        , ( "headerExpandedStyles", Code.list [] )
                                        , ( "headerClosedStyles", Code.list [] )
                                        , ( "contentStyles", Code.list [] )
                                        ]
                                        2
                                ]
                                1
                  }
                ]
        }
    , Heading.h2
        [ Heading.plaintext "Examples"
        , Heading.css
            [ Css.marginTop Spacing.verticalSpacerPx
            , Css.marginBottom (Css.px 20)
            ]
        ]
    , Accordion.view
        { entries =
            [ AccordionEntry
                { caret = Tuple.second settings_.icon
                , content = \() -> Tuple.second settings_.content
                , entryClass = "customizable-example"
                , expansionDirection = Tuple.second settings_.expansionDirection
                , headerContent = Tuple.second settings_.headerContent
                , headerId = "customizable-example-header"
                , headerLevel = Accordion.H3
                , isExpanded = Set.member 4 model.expanded
                , toggle = Just (Toggle 4)
                }
                []
            , AccordionEntry
                { caret = Accordion.defaultCaret
                , content = \_ -> Html.text "🍎 There are many kinds of apples! Apples are more genetically diverse than humans. The genetic diversity of apples means that to preserve delicious apple varieties, growers must use grafting rather than seeds. In the apple market, clones have already taken over! 🍏"
                , entryClass = "accordion-example"
                , expansionDirection = Accordion.Downwards
                , headerContent = Html.text "Apples (has children)"
                , headerId = "accordion-entry__1"
                , headerLevel = Accordion.H3
                , isExpanded = Set.member 1 model.expanded
                , toggle = Just (Toggle 1)
                }
                [ AccordionEntry
                    { caret = Accordion.defaultCaret
                    , content =
                        \_ ->
                            Html.div []
                                [ Html.img "Basket of Gala Apples"
                                    [ css [ Css.maxWidth (Css.px 100), Css.maxHeight (Css.px 100) ]
                                    , src "https://upload.wikimedia.org/wikipedia/commons/thumb/4/4b/Malus-Gala.jpg/1205px-Malus-Gala.jpg"
                                    ]
                                , Html.a [ Attributes.href "https://en.wikipedia.org/wiki/Gala_(apple)" ]
                                    [ Html.text "Wikipedia article on Gala Apples" ]
                                ]
                    , entryClass = "accordion-example-child"
                    , expansionDirection = Accordion.Downwards
                    , headerContent = Html.text "Gala"
                    , headerId = "accordion-entry__11"
                    , headerLevel = Accordion.H4
                    , isExpanded = Set.member 11 model.expanded
                    , toggle = Just (Toggle 11)
                    }
                    []
                , AccordionEntry
                    { caret = Accordion.defaultCaret
                    , content =
                        \_ ->
                            Html.div []
                                [ Html.img "Freshly-washed Granny Smith Apple"
                                    [ css [ Css.maxWidth (Css.px 100), Css.maxHeight (Css.px 100) ]
                                    , src "https://upload.wikimedia.org/wikipedia/commons/thumb/0/0e/One_Green_Apple.jpg/1280px-One_Green_Apple.jpg"
                                    ]
                                , Html.a [ Attributes.href "https://en.wikipedia.org/wiki/Granny_Smith" ]
                                    [ Html.text "Wikipedia article on Granny Smith Apples" ]
                                ]
                    , entryClass = "accordion-example-child"
                    , expansionDirection = Accordion.Downwards
                    , headerContent = Html.text "Granny Smith"
                    , headerId = "accordion-entry__12"
                    , headerLevel = Accordion.H4
                    , isExpanded = Set.member 12 model.expanded
                    , toggle = Just (Toggle 12)
                    }
                    []
                , AccordionEntry
                    { caret = Accordion.defaultCaret
                    , content =
                        \_ ->
                            Html.div []
                                [ Html.img "3 Fuji Apples resting on gingham fabric"
                                    [ css [ Css.maxWidth (Css.px 100), Css.maxHeight (Css.px 100) ]
                                    , src "https://upload.wikimedia.org/wikipedia/commons/thumb/8/81/Fuji_apples.jpg/1920px-Fuji_apples.jpg"
                                    ]
                                , Html.a [ Attributes.href "https://en.wikipedia.org/wiki/Fuji_(apple)" ]
                                    [ Html.text "Wikipedia article on Fuji Apples" ]
                                ]
                    , entryClass = "accordion-example-child"
                    , expansionDirection = Accordion.Downwards
                    , headerContent = Html.text "Fuji"
                    , headerId = "accordion-entry__13"
                    , headerLevel = Accordion.H4
                    , isExpanded = Set.member 13 model.expanded
                    , toggle = Just (Toggle 13)
                    }
                    []
                ]
            , AccordionEntry
                { caret = Accordion.upwardCaret
                , content = \_ -> Html.text "🍊 I don't know anything about oranges! Except: YUM! 🍊"
                , entryClass = "accordion-example"
                , expansionDirection = Accordion.Upwards
                , headerContent = Html.text "Oranges (upward accordion)"
                , headerId = "accordion-entry__2"
                , headerLevel = Accordion.H3
                , isExpanded = Set.member 2 model.expanded
                , toggle = Just (Toggle 2)
                }
                []
            , AccordionEntry
                { caret =
                    \isOpen ->
                        (if isOpen then
                            UiIcon.tree

                         else
                            UiIcon.sprout
                        )
                            |> Svg.withWidth (Css.px 30)
                            |> Svg.withCss [ Css.marginRight (Css.px 8) ]
                            |> Svg.toHtml
                , content =
                    \_ ->
                        Html.div
                            [ css
                                [ Css.backgroundColor Colors.gray92
                                , Css.minHeight (Css.vh 100)
                                , Css.padding (Css.px 20)
                                ]
                            ]
                            [ Html.a [ Attributes.href "https://en.wikipedia.org/wiki/Apple#/media/File:95apple.jpeg" ]
                                [ Html.img "Wild Apple" [ src "https://upload.wikimedia.org/wikipedia/commons/9/92/95apple.jpeg" ] ]
                            ]
                , entryClass = "fixed-positioning-accordion-example"
                , expansionDirection = Accordion.Downwards
                , headerContent = Html.text "Advanced Example: Expand & Scroll!"
                , headerId = "accordion-entry__6"
                , headerLevel = Accordion.H3
                , isExpanded = Set.member 6 model.expanded
                , toggle = Just (Toggle 6)
                }
                []
            ]
        , focus = Focus
        }
    , Accordion.styleAccordion
        { entryStyles =
            [ Css.Global.withClass "fixed-positioning-accordion-example"
                [ Css.marginLeft (Css.px -20)
                , Css.position Css.relative
                ]
            , Css.Global.withClass "accordion-example-child"
                [ Css.marginLeft (Css.px 16) ]
            ]
        , entryExpandedStyles =
            [ Css.Global.withClass "fixed-positioning-accordion-example"
                [ Css.Global.children
                    [ Css.Global.h4
                        [ Css.position Css.sticky
                        , Css.property "position" "-webkit-sticky"
                        , Css.top (Css.px -8)
                        ]
                    ]
                ]
            ]
        , entryClosedStyles = []
        , headerStyles =
            [ Css.Global.withClass "fixed-positioning-accordion-example"
                [ Css.padding (Css.px 20)
                ]
            ]
        , headerExpandedStyles =
            [ Css.Global.withClass "fixed-positioning-accordion-example"
                [ Css.backgroundColor Colors.gray96
                , Css.borderRadius (Css.px 8)
                , Css.boxShadow5 Css.zero Css.zero (px 10) zero (ColorsExtra.withAlpha 0.2 Colors.gray20)
                , Css.pseudoClass "focus-visible"
                    [ FocusRing.insetBoxShadows
                        [ "0 0 10px 0 " ++ ColorsExtra.toCssString (ColorsExtra.withAlpha 0.2 Colors.gray20)
                        ]
                    ]
                ]
            ]
        , headerClosedStyles = []
        , contentStyles = []
        }
    ]


{-| -}
init : State
init =
    { settings = initSettings
    , expanded = Set.empty
    }


{-| -}
type alias State =
    { settings : Control Settings
    , expanded : Set Int
    }


type alias Settings =
    { icon : ( String, Bool -> Html Msg )
    , expansionDirection : ( String, Accordion.ExpansionDirection )
    , headerContent : ( String, Html Msg )
    , content : ( String, Html Msg )
    }


initSettings : Control Settings
initSettings =
    Control.record Settings
        |> Control.field "icon" controlIcon
        |> Control.field "expansionDirection" controlExpansionDirection
        |> Control.field "headerContent" controlHeaderContent
        |> Control.field "content" controlContent


controlIcon : Control ( String, Bool -> Html Msg )
controlIcon =
    Control.choice
        [ ( "defaultCaret"
          , Control.value ( "Accordion.defaultCaret", Accordion.defaultCaret )
          )
        , ( "upwardCaret"
          , Control.value ( "Accordion.upwardCaret", Accordion.upwardCaret )
          )
        , ( "none", Control.value ( "\\_ -> text \"\"", \_ -> Html.text "" ) )
        , ( "UiIcon"
          , Control.map
                (\( code, icon ) ->
                    ( Code.newlineWithIndent 5
                        ++ Code.anonymousFunction "_"
                            (Code.newlineWithIndent 6
                                ++ Code.pipelineMultiline
                                    [ code
                                    , "Svg.withWidth (Css.px 20)"
                                    , "Svg.withHeight (Css.px 20)"
                                    , "Svg.withCss [ Css.marginRight (Css.px 8) ]"
                                    , "Svg.toHtml"
                                    ]
                                    6
                            )
                    , \_ ->
                        icon
                            |> Svg.withWidth (Css.px 20)
                            |> Svg.withHeight (Css.px 20)
                            |> Svg.withCss [ Css.marginRight (Css.px 8) ]
                            |> Svg.toHtml
                    )
                )
                (CommonControls.rotatedUiIcon 1)
          )
        ]


controlHeaderContent : Control ( String, Html Msg )
controlHeaderContent =
    Control.map
        (\v -> ( quoteF "text" v, Html.text v ))
        (Control.string "Berries")


controlExpansionDirection : Control ( String, Accordion.ExpansionDirection )
controlExpansionDirection =
    Control.choice
        [ ( "Downwards"
          , Control.value ( "Accordion.Downwards", Accordion.Downwards )
          )
        , ( "Upwards"
          , Control.value ( "Accordion.Upwards", Accordion.Upwards )
          )
        ]


controlContent : Control ( String, Html Msg )
controlContent =
    Control.map
        (\v -> ( quoteF "text" v, Html.text v ))
        (Control.stringTextarea "🍓 There are many types of berries and all of them are delicious (or poisonous (or both)). Blackberries and mulberries are especially drool-worthy.")


quoteF : String -> String -> String
quoteF f v =
    f ++ " \"" ++ v ++ "\""


type Msg
    = UpdateControls (Control Settings)
    | Toggle Int Bool
    | Focus String
    | Focused (Result Dom.Error ())


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        UpdateControls settings ->
            ( { model | settings = settings }
            , Cmd.none
            )

        Toggle id expand ->
            ( if expand then
                { model | expanded = Set.insert id model.expanded }

              else
                { model | expanded = Set.remove id model.expanded }
            , Cmd.none
            )

        Focus id ->
            ( model, Task.attempt Focused (Dom.focus id) )

        Focused _ ->
            ( model, Cmd.none )
