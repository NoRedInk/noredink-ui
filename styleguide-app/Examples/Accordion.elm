module Examples.Accordion exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import Accessibility.Styled as Html exposing (Html)
import AtomicDesignType exposing (AtomicDesignType(..))
import Browser.Dom as Dom
import Category exposing (Category(..))
import Css exposing (..)
import Css.Global
import Example exposing (Example)
import Html.Styled.Attributes as Attributes exposing (css, src)
import KeyboardSupport exposing (Direction(..), Key(..))
import Nri.Ui.Accordion.V3 as Accordion exposing (AccordionEntry(..))
import Nri.Ui.Colors.Extra as ColorsExtra
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.DisclosureIndicator.V2 as DisclosureIndicator
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Text.V4 as Text
import Nri.Ui.UiIcon.V1 as UiIcon
import Set exposing (Set)
import Task


{-| -}
example : Example State Msg
example =
    { name = "Accordion"
    , version = 3
    , state = init
    , update = update
    , subscriptions = \_ -> Sub.none
    , view = view
    , categories = [ Layout ]
    , atomicDesignType = Molecule
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
view : State -> List (Html Msg)
view model =
    let
        defaultCaret =
            DisclosureIndicator.large [ Css.marginRight (Css.px 8) ]
    in
    [ Heading.h3 [] [ Html.text "Accordion.view" ]
    , Accordion.view
        { entries =
            [ AccordionEntry
                { caret = defaultCaret
                , content = \_ -> Html.text "🍎 There are many kinds of apples! Apples are more genetically diverse than humans. The genetic diversity of apples means that to preserve delicious apple varieties, growers must use grafting rather than seeds. In the apple market, clones have already taken over! 🍏"
                , entryClass = "accordion-example"
                , headerContent = Html.text "Apples"
                , headerId = "accordion-entry__1"
                , headerLevel = Accordion.H4
                , isExpanded = Set.member 1 model
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
                    , isExpanded = Set.member 11 model
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
                    , isExpanded = Set.member 12 model
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
                    , isExpanded = Set.member 13 model
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
                , isExpanded = Set.member 2 model
                , toggle = Just (Toggle 2)
                }
                []
            , AccordionEntry
                { caret = defaultCaret
                , content = \_ -> Html.text "There are many types of berries and all of them are delicious (or poisonous (or both)). Blackberries and mulberries are especially drool-worthy."
                , entryClass = "accordion-example"
                , headerContent = Html.text "Berries"
                , headerId = "accordion-entry__4"
                , headerLevel = Accordion.H5
                , isExpanded = Set.member 4 model
                , toggle = Just (Toggle 4)
                }
                []
            , AccordionEntry
                { caret = defaultCaret
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
                , headerContent = Html.text "Fixed Position Example: Expand & Scroll!"
                , headerId = "accordion-entry__6"
                , headerLevel = Accordion.H4
                , isExpanded = Set.member 6 model
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
                ]
            ]
        , headerClosedStyles = []
        , contentStyles = []
        }
    ]


type Msg
    = Toggle Int Bool
    | Focus String
    | Focused (Result Dom.Error ())


{-| -}
init : State
init =
    Set.empty


{-| -}
type alias State =
    Set Int


{-| -}
update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        Toggle id expand ->
            ( if expand then
                Set.insert id model

              else
                Set.remove id model
            , Cmd.none
            )

        Focus id ->
            ( model, Task.attempt Focused (Dom.focus id) )

        Focused _ ->
            ( model, Cmd.none )
