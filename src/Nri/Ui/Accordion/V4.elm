module Nri.Ui.Accordion.V4 exposing
    ( view, HeaderLevel(..)
    , AccordionEntry(..), Entry
    , StyleOptions, styleAccordion
    )

{-|


## Changes from V3

  - Adds optional `role="region"` to the panels


## Example

    import Nri.Ui.DisclosureIndicator.V2 as DisclosureIndicator

    view : Model -> Html Msg
    view model =
        div []
            [ Accordion.view
                { entries =
                    [ AccordionEntry
                        { caret = \isOpen -> DisclosureIndicator.large [ marginRight (px 8) ] isOpen
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

-}

import Accessibility.Styled exposing (Html, button, div, section)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Landmark as Landmark
import Css exposing (..)
import Css.Global
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events exposing (onClick)
import Html.Styled.Keyed
import Json.Decode as Decode
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Html.Attributes.V2 as AttributesExtra


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
    , panelAsRegion : Bool
    }
    -> Html msg
view { entries, focus, panelAsRegion } =
    view_
        { entries = entries
        , focus = focus
        , leftId = Nothing
        , panelAsRegion = panelAsRegion
        }


view_ :
    { entries : List (AccordionEntry msg)
    , focus : String -> msg
    , leftId : Maybe String
    , panelAsRegion : Bool
    }
    -> Html msg
view_ { entries, focus, leftId, panelAsRegion } =
    let
        headerIds : List String
        headerIds =
            List.map getHeaderId entries

        arrowUpIds : List (Maybe String)
        arrowUpIds =
            lastHeaderId :: List.map Just headerIds

        firstHeaderId : Maybe String
        firstHeaderId =
            List.head headerIds

        lastHeaderId : Maybe String
        lastHeaderId =
            List.head (List.reverse headerIds)
    in
    div
        [ Attributes.class accordionClass
        , Attributes.attribute "aria-live" "polite"
        ]
        [ Html.Styled.Keyed.node "div"
            []
            (entries
                |> List.map2 (\id nextEntry -> ( id, nextEntry )) arrowUpIds
                |> List.foldr
                    (\( previousId, AccordionEntry entry_ children ) ( nextId, acc ) ->
                        let
                            node =
                                ( "keyed-section__" ++ entry_.headerId
                                , viewEntry panelAsRegion
                                    focus
                                    { up = previousId
                                    , down = nextId
                                    , right = Maybe.map getHeaderId (List.head children)
                                    , left = leftId
                                    }
                                    entry_
                                    children
                                )
                        in
                        ( Just entry_.headerId
                        , node :: acc
                        )
                    )
                    ( firstHeaderId, [] )
                |> Tuple.second
            )
        ]


viewEntry :
    Bool
    -> (String -> msg)
    ->
        { up : Maybe String
        , down : Maybe String
        , right : Maybe String
        , left : Maybe String
        }
    -> Entry msg
    -> List (AccordionEntry msg)
    -> Html msg
viewEntry panelAsRegion focus arrows ({ headerId, headerLevel, caret, headerContent, entryClass, content, isExpanded } as config) children =
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
                    ]
                , Aria.disabled (config.toggle == Nothing)
                , Aria.expanded isExpanded
                , Aria.controls [ panelId ]
                , config.toggle
                    |> Maybe.map (\toggle -> onClick (toggle (not isExpanded)))
                    |> Maybe.withDefault AttributesExtra.none
                , Events.custom "keydown"
                    ([ Maybe.map (\id -> Key.up (focus id)) arrows.up
                     , Maybe.map (\id -> Key.down (focus id)) arrows.down
                     , Maybe.map (\id -> Key.right (focus id)) arrows.right
                     , Maybe.map (\id -> Key.left (focus id)) arrows.left
                     ]
                        |> List.filterMap identity
                        |> Decode.oneOf
                        |> Decode.map
                            (\event ->
                                { message = event
                                , stopPropagation = False
                                , preventDefault = True
                                }
                            )
                    )
                ]
                [ caret isExpanded
                , headerContent
                ]
        , section
            [ Attributes.id panelId
            , if panelAsRegion == True then
                Landmark.region

              else
                AttributesExtra.none
            , Aria.labelledBy headerId
            , Attributes.classList
                [ ( accordionEntryPanelClass, True )
                , ( entryClass, True )
                ]
            , Attributes.hidden (not isExpanded)
            ]
            (if isExpanded then
                [ content ()
                , view_ { focus = focus, entries = children, leftId = Just headerId, panelAsRegion = panelAsRegion }
                ]

             else
                []
            )
        ]
