module Nri.Ui.Accordion.V3 exposing
    ( view, HeaderLevel(..)
    , AccordionEntry(..), Entry
    , StyleOptions, styleAccordion
    , defaultCaret
    )

{-|


## Changes from V2

  - Do not render collapsed content
  - Allow disclosure indicator to be top-aligned
  - Allow fully-customizable carets
  - Removes as many default styles as possible
  - Change header resets to inline styles
  - Replace custom passed-in styles with a class-based styling approach (for performance reasons -- getting elm-css class names is too slow.)
  - Prevent default browser event on arrow keys (so that the page doesn't scroll when the focus changes)
  - Support multiple levels of accordions


## Example

    view : Model -> Html Msg
    view model =
        div []
            [ Accordion.view
                { entries =
                    [ AccordionEntry
                        { caret = Accordion.defaultCaret
                        , content = \() -> text "Accordion Content"
                        , entryClass = "a-class-distinguishing-this-accordion-from-others-on-the-page"
                        , headerContent = text "Accordion Header"
                        , headerId = "a-unique-id-for-this-accordion-header-button"
                        , headerLevel = Accordion.H1
                        , isExpanded = model.isAccordionOpen
                        , toggle = Just ToggleAccordion
                        }
                        []
                    ]
                , focus = Focus
                }
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

@docs view, HeaderLevel
@docs AccordionEntry, Entry
@docs StyleOptions, styleAccordion
@docs defaultCaret

-}

import Accessibility.Styled exposing (Html, button, div, section)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Css exposing (..)
import Css.Global
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed
import Nri.Ui.AnimatedIcon.V1 as AnimatedIcon
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusLoop.V1 as FocusLoop
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Svg.V1 as Svg


{-| -}
defaultCaret : Bool -> Html msg
defaultCaret isOpen =
    AnimatedIcon.arrowRightDown isOpen
        |> Svg.withColor Colors.azure
        |> Svg.withWidth (Css.px 17)
        |> Svg.withHeight (Css.px 17)
        |> Svg.withCss [ Css.marginRight (Css.px 8) ]
        |> Svg.toHtml


{-| -}
type alias StyleOptions =
    { entryStyles : List Style
    , entryExpandedStyles : List Style
    , entryClosedStyles : List Style
    , headerStyles : List Style
    , headerExpandedStyles : List Style
    , headerClosedStyles : List Style
    , contentStyles : List Style
    }


{-| -}
styleAccordion : StyleOptions -> Html msg
styleAccordion styleOptions =
    Css.Global.global
        [ Css.Global.class accordionHeaderClass
            [ margin zero, padding zero ]
        , Css.Global.class accordionEntryHeaderClass
            ([ displayFlex
             , alignItems center
             , boxSizing borderBox
             , minWidth (pct 100)

             -- button resets
             , Css.Global.withAttribute "aria-disabled=false" [ cursor pointer ]
             , Css.backgroundColor Css.unset
             , borderWidth Css.zero
             , margin zero
             , Css.pseudoClass "focus-visible" [ FocusRing.insetBoxShadows [] ]

             -- fonts & text
             , textAlign left
             , Fonts.baseFont
             , fontSize (px 16)
             , fontWeight (int 600)
             , lineHeight (num 1.2)
             , Css.Global.withClass accordionEntryHeaderExpandedClass
                styleOptions.headerExpandedStyles
             , Css.Global.withClass accordionEntryHeaderCollapsedClass
                styleOptions.headerClosedStyles
             ]
                ++ styleOptions.headerStyles
            )
        , Css.Global.class accordionEntryClass
            ([ marginBottom (px 10)
             , Css.Global.withClass accordionEntryExpandedClass
                styleOptions.entryExpandedStyles
             , Css.Global.withClass accordionEntryCollapsedClass
                styleOptions.entryClosedStyles
             ]
                ++ styleOptions.entryStyles
            )
        , Css.Global.class accordionEntryPanelClass
            styleOptions.contentStyles
        ]


accordionClass : String
accordionClass =
    "accordion-v3"


accordionHeaderClass : String
accordionHeaderClass =
    "accordion-v3-header"


accordionEntryHeaderClass : String
accordionEntryHeaderClass =
    "accordion-v3-entry-header"


accordionEntryHeaderExpandedClass : String
accordionEntryHeaderExpandedClass =
    "accordion-v3-entry-header-expanded"


accordionEntryHeaderCollapsedClass : String
accordionEntryHeaderCollapsedClass =
    "accordion-v3-entry-header-collapsed"


accordionEntryClass : String
accordionEntryClass =
    "accordion-v3-entry"


accordionEntryExpandedClass : String
accordionEntryExpandedClass =
    "accordion-v3-entry-state-expanded"


accordionEntryCollapsedClass : String
accordionEntryCollapsedClass =
    "accordion-v3-entry-state-collapsed"


accordionEntryPanelClass : String
accordionEntryPanelClass =
    "accordion-v3-entry-panel"


{-| Corresponds to h1, h2, h3 etc.
Choose the correct header level given your page context.
-}
type HeaderLevel
    = H1
    | H2
    | H3
    | H4
    | H5
    | H6


header : HeaderLevel -> Html msg -> Html msg
header headerLevel content =
    case headerLevel of
        H1 ->
            Accessibility.Styled.h1 [ Attributes.class accordionHeaderClass ] [ content ]

        H2 ->
            Accessibility.Styled.h2 [ Attributes.class accordionHeaderClass ] [ content ]

        H3 ->
            Accessibility.Styled.h3 [ Attributes.class accordionHeaderClass ] [ content ]

        H4 ->
            Accessibility.Styled.h4 [ Attributes.class accordionHeaderClass ] [ content ]

        H5 ->
            Accessibility.Styled.h5 [ Attributes.class accordionHeaderClass ] [ content ]

        H6 ->
            Accessibility.Styled.h6 [ Attributes.class accordionHeaderClass ] [ content ]


{-| -}
type AccordionEntry msg
    = AccordionEntry (Entry msg) (List (AccordionEntry msg))


{-| -}
type alias Entry msg =
    { caret : Bool -> Html msg
    , content : () -> Html msg
    , entryClass : String
    , headerContent : Html msg
    , headerId : String
    , headerLevel : HeaderLevel
    , isExpanded : Bool
    , toggle : Maybe (Bool -> msg)
    }


getHeaderId : AccordionEntry msg -> String
getHeaderId entry =
    case entry of
        AccordionEntry { headerId } _ ->
            headerId


{-| -}
view :
    { entries : List (AccordionEntry msg)
    , focus : String -> msg
    }
    -> Html msg
view { entries, focus } =
    view_
        { entries = entries
        , focus = focus
        , leftId = Nothing
        }


view_ :
    { entries : List (AccordionEntry msg)
    , focus : String -> msg
    , leftId : Maybe String
    }
    -> Html msg
view_ { entries, focus, leftId } =
    div [ Attributes.class accordionClass ]
        [ Html.Styled.Keyed.node "div"
            []
            (FocusLoop.addEvents
                { focus = \(AccordionEntry { headerId } _) -> focus headerId
                , leftRight = False
                , upDown = True
                }
                entries
                |> List.map
                    (\( AccordionEntry entry_ children, upDownEvents ) ->
                        ( "keyed-section__" ++ entry_.headerId
                        , viewEntry focus
                            { upDownEvents = upDownEvents
                            , right = Maybe.map getHeaderId (List.head children)
                            , left = leftId
                            }
                            entry_
                            children
                        )
                    )
            )
        ]


viewEntry :
    (String -> msg)
    ->
        { upDownEvents : List (Key.Event msg)
        , right : Maybe String
        , left : Maybe String
        }
    -> Entry msg
    -> List (AccordionEntry msg)
    -> Html msg
viewEntry focus arrows ({ headerId, headerLevel, caret, headerContent, entryClass, content, isExpanded } as config) children =
    let
        panelId =
            "accordion-panel__" ++ headerId
    in
    div
        [ Attributes.classList
            [ ( accordionEntryClass, True )
            , ( entryClass, True )
            , ( accordionEntryExpandedClass, isExpanded )
            , ( accordionEntryCollapsedClass, not isExpanded )
            ]
        ]
        [ header headerLevel <|
            button
                [ Attributes.id headerId
                , Attributes.classList
                    [ ( accordionEntryHeaderClass, True )
                    , ( entryClass, True )
                    , ( accordionEntryHeaderExpandedClass, isExpanded )
                    , ( accordionEntryHeaderCollapsedClass, not isExpanded )
                    , ( FocusRing.customClass, True )
                    ]
                , Aria.disabled (config.toggle == Nothing)
                , Aria.expanded isExpanded
                , Aria.controls [ panelId ]
                , config.toggle
                    |> Maybe.map (\toggle -> onClick (toggle (not isExpanded)))
                    |> Maybe.withDefault AttributesExtra.none
                , Key.onKeyDownPreventDefault
                    (arrows.upDownEvents
                        ++ List.filterMap identity
                            [ Maybe.map (\id -> Key.right (focus id)) arrows.right
                            , Maybe.map (\id -> Key.left (focus id)) arrows.left
                            ]
                    )
                ]
                [ caret isExpanded
                , headerContent
                ]
        , section
            [ Attributes.id panelId
            , Aria.labelledBy headerId
            , Attributes.classList
                [ ( accordionEntryPanelClass, True )
                , ( entryClass, True )
                ]
            , Attributes.hidden (not isExpanded)
            ]
            (if isExpanded then
                [ content ()
                , view_ { focus = focus, entries = children, leftId = Just headerId }
                ]

             else
                []
            )
        ]
