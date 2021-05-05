module Examples.Accordion exposing
    ( example
    , State, Msg
    )

{-|

@docs example
@docs State, Msg

-}

import AtomicDesignType exposing (AtomicDesignType(..))
import Browser.Dom as Dom
import Category exposing (Category(..))
import Css exposing (..)
import Css.Global
import Example exposing (Example)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes exposing (css)
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

        defaultClass =
            "first-accordion-example"
    in
    [ Heading.h3 [] [ Html.text "Accordion.view" ]
    , Accordion.view
        { entries =
            [ AccordionEntry
                { caret = defaultCaret
                , content = \_ -> Html.text "Content for the first accordion"
                , entryClass = defaultClass
                , headerContent = Html.text "Default Look"
                , headerId = "accordion-entry__1"
                , headerLevel = Accordion.H4
                , isExpanded = Set.member 1 model
                , toggle = Just (Toggle 1)
                }
                [ AccordionEntry
                    { caret = defaultCaret
                    , content = \_ -> Html.text "Content for the Accordion Child"
                    , entryClass = defaultClass
                    , headerContent = Html.text "Accordion Child"
                    , headerId = "accordion-entry__2"
                    , headerLevel = Accordion.H5
                    , isExpanded = Set.member 2 model
                    , toggle = Just (Toggle 2)
                    }
                    []
                ]
            , AccordionEntry
                { caret =
                    \isOpen ->
                        UiIcon.seeMore
                            |> Svg.withWidth (Css.px 17)
                            |> Svg.withHeight (Css.px 17)
                            |> Svg.withCss [ Css.marginRight (Css.px 8) ]
                            |> Svg.withColor
                                (if isOpen then
                                    Colors.azureDark

                                 else
                                    Colors.azure
                                )
                            |> Svg.toHtml
                , content = \_ -> Html.text "Content for the third accordion"
                , entryClass = defaultClass
                , headerContent = Html.text "Custom Carets!"
                , headerId = "accordion-entry__3"
                , headerLevel = Accordion.H4
                , isExpanded = Set.member 3 model
                , toggle = Just (Toggle 3)
                }
                []
            , AccordionEntry
                { caret =
                    \_ ->
                        UiIcon.null
                            |> Svg.withWidth (Css.px 17)
                            |> Svg.withHeight (Css.px 17)
                            |> Svg.withCss [ Css.marginRight (Css.px 8) ]
                            |> Svg.withColor Colors.gray85
                            |> Svg.toHtml
                , content = \_ -> Html.text "Content for the fourth accordion"
                , entryClass = defaultClass
                , headerContent = Html.text "This accordion can't be opened."
                , headerId = "accordion-entry__4"
                , headerLevel = Accordion.H5
                , isExpanded = Set.member 4 model
                , toggle = Nothing
                }
                []
            , AccordionEntry
                { caret =
                    \_ ->
                        UiIcon.null
                            |> Svg.withWidth (Css.px 17)
                            |> Svg.withHeight (Css.px 17)
                            |> Svg.withCss [ Css.marginRight (Css.px 8) ]
                            |> Svg.withColor Colors.gray85
                            |> Svg.toHtml
                , content = \_ -> Html.text "Content for the fifth accordion"
                , entryClass = defaultClass
                , headerContent = Html.text "This accordion can't be closed."
                , headerId = "accordion-entry__5"
                , headerLevel = Accordion.H5
                , isExpanded = True
                , toggle = Nothing
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
                            [ Html.text "Content for the accordion"
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
            [ Css.marginLeft (Css.px 16)
            , Css.Global.withClass "fixed-positioning-accordion-example"
                [ Css.position Css.relative
                ]
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
