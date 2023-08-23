module Examples.FocusRing exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Browser.Dom
import Category exposing (Category(..))
import Css exposing (Style)
import Example exposing (Example)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css, href)
import Markdown
import Nri.Ui.Accordion.V3 as Accordion exposing (AccordionEntry(..))
import Nri.Ui.Button.V10 as Button
import Nri.Ui.Checkbox.V7 as Checkbox
import Nri.Ui.ClickableSvg.V2 as ClickableSvg
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.RadioButton.V4 as RadioButton
import Nri.Ui.SegmentedControl.V14 as SegmentedControl
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Svg.V1 as Svg
import Nri.Ui.Switch.V3 as Switch
import Nri.Ui.Table.V7 as Table
import Nri.Ui.UiIcon.V1 as UiIcon
import Task


{-| -}
example : Example State Msg
example =
    { name = "FocusRing"
    , version = 1
    , categories = [ Atoms ]
    , keyboardSupport = []
    , state = { isAccordionOpen = False }
    , update = update
    , subscriptions = \_ -> Sub.none
    , preview =
        [ example_ (Css.marginBottom (Css.px 30) :: FocusRing.styles)
        , example_ FocusRing.tightStyles
        ]
    , about =
        [ text "Custom high-contrast focus ring styles. Learn more about this component in "
        , ClickableText.link "Custom Focus Rings on the NoRedInk blog"
            [ ClickableText.linkExternal "https://blog.noredink.com/post/703458632758689792/custom-focus-rings"
            , ClickableText.appearsInline
            ]
        , text "."
        ]
    , view =
        \_ state ->
            [ Heading.h2
                [ Heading.plaintext "Examples"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , viewTable state
            , Accordion.styleAccordion
                { entryStyles = []
                , entryExpandedStyles = []
                , entryClosedStyles = []
                , headerStyles = []
                , headerExpandedStyles = []
                , headerClosedStyles = []
                , contentStyles = []
                }
            ]
    }


example_ : List Style -> Html msg
example_ styles =
    div
        [ css
            [ Css.width (Css.pct 100)
            , Css.height (Css.px 20)
            , Css.batch styles
            ]
        ]
        []


exampleWithBorderAndBG : List Style -> Html msg
exampleWithBorderAndBG styles =
    example_
        (Css.border3 (Css.px 2) Css.dashed Colors.gray20
            :: Css.backgroundColor Colors.gray75
            :: styles
        )


viewTable : State -> Html Msg
viewTable model =
    Table.view []
        [ Table.rowHeader
            { header = Html.text "Name"
            , view = \{ name } -> code [] [ text name ]
            , width = Css.pct 10
            , cellStyles =
                always
                    [ Css.textAlign Css.left
                    , Css.padding2 (Css.px 8) (Css.px 16)
                    ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "View"
            , view = .view
            , width = Css.pct 10
            , cellStyles = always []
            , sort = Nothing
            }
        , Table.custom
            { header = text "About"
            , view =
                .about
                    >> Markdown.toHtml Nothing
                    >> List.map fromUnstyled
                    >> div []
            , width = Css.px 300
            , cellStyles =
                always
                    [ Css.padding2 (Css.px 8) (Css.px 16)
                    , Css.verticalAlign Css.top
                    ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Two-toned"
            , view =
                \{ twoToned } ->
                    if twoToned then
                        UiIcon.checkmark
                            |> Svg.withLabel "Yes"
                            |> Svg.withWidth (Css.px 20)
                            |> Svg.withColor Colors.greenDark
                            |> Svg.toHtml

                    else
                        UiIcon.x
                            |> Svg.withLabel "No"
                            |> Svg.withWidth (Css.px 20)
                            |> Svg.withColor Colors.red
                            |> Svg.toHtml
            , width = Css.pct 1
            , cellStyles = always [ Css.padding2 (Css.px 8) (Css.px 16) ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "Examples"
            , view =
                .examples
                    >> div
                        [ css
                            [ Css.displayFlex
                            , Css.flexDirection Css.column
                            , Css.property "gap" "10px"
                            ]
                        ]
            , width = Css.pct 10
            , cellStyles = always [ Css.padding2 (Css.px 8) (Css.px 16) ]
            , sort = Nothing
            }
        ]
        [ { name = "styles"
          , view = exampleWithBorderAndBG FocusRing.styles
          , twoToned = True
          , examples =
                [ button [] [ text "Button" ]
                , label [] [ input [] [], text "Input" ]
                , a [ href "#" ] [ text "Link" ]
                ]
          , about =
                """
A two-tone focus ring that will be visually apparent for any background/element combination.

NOTE: use `boxShadows` instead if your focusable element:

  - already has a box shadow
  - has an explicit border radius set
"""
          }
        , { name = "tightStyles"
          , view = exampleWithBorderAndBG FocusRing.tightStyles
          , twoToned = True
          , examples =
                [ ClickableText.button "ClickableText button" [ ClickableText.small ]
                , ClickableText.link "ClickableText link" [ ClickableText.small ]
                , RadioButton.view
                    { label = "Radio button"
                    , name = "radio-input"
                    , value = "Selected"
                    , selectedValue = Just "Selected"
                    , valueToString = identity
                    }
                    []
                , Checkbox.view
                    { label = "Checkbox"
                    , selected = Checkbox.Selected
                    }
                    []
                , p [] [ a [ href "#" ] [ text "`a` in a `p`" ] ]
                ]
          , about = "Prefer `styles` over tightStyles, except in cases where line spacing/font size will otherwise cause obscured content."
          }
        , { name = "boxShadows"
          , view = exampleWithBorderAndBG [ FocusRing.boxShadows [] ]
          , twoToned = True
          , examples =
                [ Switch.view { label = "Switch", id = "switch" } []
                , ClickableSvg.button "ClickableSvg button" UiIcon.playInCircle []
                , SegmentedControl.viewRadioGroup
                    { legend = "SegmentedControls.viewRadioGroup"
                    , onSelect = \_ -> NoOp
                    , options =
                        [ { icon = Nothing
                          , label = text "SegmentedControls"
                          , value = 1
                          , idString = "radio-1"
                          , tooltip = []
                          , attributes = []
                          }
                        ]
                    , selected = Nothing
                    , positioning = SegmentedControl.Left SegmentedControl.FitContent
                    }
                , Button.button "Button button" [ Button.small, Button.secondary ]
                ]
          , about = ""
          }
        , { name = "insetBoxShadows"
          , view = exampleWithBorderAndBG [ FocusRing.insetBoxShadows [] ]
          , twoToned = True
          , examples =
                [ Accordion.view
                    { entries =
                        [ AccordionEntry
                            { caret = Accordion.defaultCaret
                            , content = \() -> text "Content"
                            , entryClass = "accordion-focus-ring"
                            , headerContent = text "Accordion"
                            , headerId = "accordion-focus-ring-example-header"
                            , headerLevel = Accordion.H3
                            , isExpanded = model.isAccordionOpen
                            , toggle = Just ToggleAccordion
                            }
                            []
                        ]
                    , focus = Focus
                    }
                ]
          , about = "Please be sure that the padding on the element you add this style too is sufficient (at least 6px on all sides) that the inset box shadow won't cover any content."
          }
        , { name = "outerBoxShadow"
          , view = exampleWithBorderAndBG [ FocusRing.outerBoxShadow ]
          , twoToned = False
          , examples = [ text "(See Tabs and TabsMinimal)" ]
          , about = "In special cases, we don't use a two-tone focus ring. Be very sure this is what you need before using this!"
          }
        , { name = "insetBoxShadow"
          , view = exampleWithBorderAndBG [ FocusRing.insetBoxShadow ]
          , twoToned = False
          , examples = [ text "(See SideNav)" ]
          , about = "In special cases, we don't use a two-tone focus ring, and an outset focus ring would be obscured. Be very sure this is what you need before using this!"
          }
        , { name = "outerColor"
          , view =
                example_
                    [ Css.border3 (Css.px 2) Css.solid Colors.gray20
                    , Css.backgroundColor FocusRing.outerColor
                    ]
          , twoToned = False
          , examples = []
          , about = "Colors.red"
          }
        , { name = "innerColor"
          , view =
                example_
                    [ Css.border3 (Css.px 2) Css.solid Colors.gray20
                    , Css.backgroundColor FocusRing.innerColor
                    ]
          , twoToned = False
          , examples = []
          , about = "Colors.white"
          }
        ]


{-| -}
type alias State =
    { isAccordionOpen : Bool }


{-| -}
type Msg
    = ToggleAccordion Bool
    | Focus String
    | NoOp


update : Msg -> State -> ( State, Cmd Msg )
update msg model =
    case msg of
        ToggleAccordion isOpen ->
            ( { model | isAccordionOpen = isOpen }
            , Cmd.none
            )

        Focus id ->
            ( model
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus id)
            )

        NoOp ->
            ( model, Cmd.none )
