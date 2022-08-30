module Examples.Accordion exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import Accessibility.Styled as Html exposing (Html)
import Browser.Dom as Dom
import Category exposing (Category(..))
import CommonControls
import Css exposing (..)
import Css.Global
import Debug.Control as Control exposing (Control)
import Debug.Control.View as ControlView
import EllieLink
import Example exposing (Example)
import Html.Styled.Attributes as Attributes exposing (css, src)
import KeyboardSupport exposing (Key(..))
import Nri.Ui.Accordion.V3 as Accordion exposing (AccordionEntry(..))
import Nri.Ui.Colors.Extra as ColorsExtra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.DisclosureIndicator.V2 as DisclosureIndicator
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V6 as Text
import Nri.Ui.UiIcon.V1 as UiIcon
import Set exposing (Set)
import Task


moduleName : String
moduleName =
    "Accordion"


version : Int
version =
    3


{-| -}
example : Example State Msg
example =
    { name = moduleName
    , version = version
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ -- faking a mini version of the Accordion component to give styleguide users a sense of what the
          -- component might look like
          Html.div []
            [ Html.div [ css [ Css.displayFlex, Css.alignItems Css.center ] ]
                [ defaultCaret False
                , Text.smallBody [ Text.plaintext "Closed" ]
                ]
            , Html.div [ css [ Css.displayFlex, Css.alignItems Css.center ] ]
                [ defaultCaret True
                , Text.smallBody [ Text.plaintext "Open" ]
                ]
            , Text.caption [ Text.plaintext "Accordion content." ]
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


defaultCaret : Bool -> Html msg
defaultCaret =
    DisclosureIndicator.large [ Css.marginRight (Css.px 8) ] >> Svg.toHtml


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
        , extraCode =
            [ "import Nri.Ui.DisclosureIndicator.V2 as DisclosureIndicator"
            , "import Nri.Ui.Svg.V1 as Svg"
            ]
        , toExampleCode =
            \settings ->
                [ { sectionName = "Partial example"
                  , code =
                        String.join "\n"
                            [ "  div [] ["
                            , "    Accordion.view"
                            , "      { entries ="
                            , "          [ Accordion.AccordionEntry"
                            , "              { caret = " ++ Tuple.first settings.icon
                            , "              , content = \\() -> " ++ Tuple.first settings.content
                            , "              , entryClass = \"customizable-example\""
                            , "              , headerContent = " ++ Tuple.first settings.headerContent
                            , "              , headerId = \"customizable-example-header\""
                            , "              , headerLevel = Accordion.H4"
                            , "              , isExpanded = True"
                            , "              , toggle = Nothing"
                            , "              }"
                            , "              []"
                            , "          ]"
                            , "      , -- When using Accordion, be sure to wire up Focus management correctly!"
                            , "        focus = identity"
                            , "      }"
                            , "    , Accordion.styleAccordion"
                            , "      { entryStyles = []"
                            , "      , entryExpandedStyles = []"
                            , "      , entryClosedStyles = []"
                            , "      , headerStyles = []"
                            , "      , headerExpandedStyles = []"
                            , "      , headerClosedStyles = []"
                            , "      , contentStyles = []"
                            , "      }"
                            , "  ]"
                            ]
                  }
                ]
        }
    , Accordion.view
        { entries =
            [ AccordionEntry
                { caret = Tuple.second settings_.icon
                , content = \() -> Tuple.second settings_.content
                , entryClass = "customizable-example"
                , headerContent = Tuple.second settings_.headerContent
                , headerId = "customizable-example-header"
                , headerLevel = Accordion.H4
                , isExpanded = Set.member 4 model.expanded
                , toggle = Just (Toggle 4)
                }
                []
            , AccordionEntry
                { caret = defaultCaret
                , content = \_ -> Html.text "🍎 There are many kinds of apples! Apples are more genetically diverse than humans. The genetic diversity of apples means that to preserve delicious apple varieties, growers must use grafting rather than seeds. In the apple market, clones have already taken over! 🍏"
                , entryClass = "accordion-example"
                , headerContent = Html.text "Apples (has children)"
                , headerId = "accordion-entry__1"
                , headerLevel = Accordion.H4
                , isExpanded = Set.member 1 model.expanded
                , toggle = Just (Toggle 1)
                }
                [ AccordionEntry
                    { caret = defaultCaret
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
                    , headerContent = Html.text "Gala"
                    , headerId = "accordion-entry__11"
                    , headerLevel = Accordion.H5
                    , isExpanded = Set.member 11 model.expanded
                    , toggle = Just (Toggle 11)
                    }
                    []
                , AccordionEntry
                    { caret = defaultCaret
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
                    , headerContent = Html.text "Granny Smith"
                    , headerId = "accordion-entry__12"
                    , headerLevel = Accordion.H5
                    , isExpanded = Set.member 12 model.expanded
                    , toggle = Just (Toggle 12)
                    }
                    []
                , AccordionEntry
                    { caret = defaultCaret
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
                    , headerContent = Html.text "Fuji"
                    , headerId = "accordion-entry__13"
                    , headerLevel = Accordion.H5
                    , isExpanded = Set.member 13 model.expanded
                    , toggle = Just (Toggle 13)
                    }
                    []
                ]
            , AccordionEntry
                { caret = defaultCaret
                , content = \_ -> Html.text "🍊 I don't know anything about oranges! Except: YUM! 🍊"
                , entryClass = "accordion-example"
                , headerContent = Html.text "Oranges"
                , headerId = "accordion-entry__2"
                , headerLevel = Accordion.H4
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
                , headerContent = Html.text "Advanced Example: Expand & Scroll!"
                , headerId = "accordion-entry__6"
                , headerLevel = Accordion.H4
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
    , headerContent : ( String, Html Msg )
    , content : ( String, Html Msg )
    }


initSettings : Control Settings
initSettings =
    Control.record Settings
        |> Control.field "icon" controlIcon
        |> Control.field "headerContent" controlHeaderContent
        |> Control.field "content" controlContent


controlIcon : Control ( String, Bool -> Html Msg )
controlIcon =
    Control.choice
        [ ( "DisclosureIndicator"
          , Control.value
                ( "DisclosureIndicator.large [ Css.marginRight (Css.px 8) ] >> Svg.toHtml"
                , DisclosureIndicator.large [ Css.marginRight (Css.px 8) ] >> Svg.toHtml
                )
          )
        , ( "none", Control.value ( "\\_ -> text \"\"", \_ -> Html.text "" ) )
        , ( "UiIcon"
          , Control.map
                (\( code, icon ) ->
                    ( "\\_ -> Svg.toHtml " ++ code
                    , \_ -> Svg.toHtml icon
                    )
                )
                CommonControls.uiIcon
          )
        ]


controlHeaderContent : Control ( String, Html Msg )
controlHeaderContent =
    Control.map
        (\v -> ( quoteF "text" v, Html.text v ))
        (Control.string "Berries")


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
