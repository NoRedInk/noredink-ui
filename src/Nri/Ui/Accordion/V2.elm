module Nri.Ui.Accordion.V2 exposing (view, Caret(..), StyleOptions, HeaderLevel(..))

{-| Changes from V1:

  - Combine view and viewKeyed so that nodes are always keyed
  - Removes viewCaret from the API -- it's possible to use the DisclosureIndicator directly
  - Changed implementation to follow recommendations from <https://www.w3.org/TR/wai-aria-practices-1.1/examples/accordion/accordion.html>
      - Adds Up and Down keyboard handling for navigating between the accordions
      - Adds aria-expanded and aria-controls
      - Changes accordion container to a section
      - removes tablist/tab/tabpanel roles
      - Adds heading levels for the accordion header (including style resets)

@docs view, Caret, StyleOptions, HeaderLevel

-}

import Accessibility.Styled exposing (Attribute, Html, button, div, section, text)
import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
import Accessibility.Styled.Widget as Widget
import Css exposing (..)
import Css.Global
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.DisclosureIndicator.V2 as DisclosureIndicator
import Nri.Ui.Fonts.V1 as Fonts


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


defaultStyleOptions : StyleOptions
defaultStyleOptions =
    { entryStyles =
        [ position relative
        , marginBottom (px 10)
        ]
    , entryExpandedStyles = []
    , entryClosedStyles = []
    , headerStyles =
        [ displayFlex
        , alignItems center
        , boxSizing borderBox
        , minWidth (pct 100)
        , overflow hidden
        , padding2 (px 8) (px 15)
        , backgroundColor Colors.gray96

        -- button resets
        , cursor pointer
        , borderWidth Css.zero
        , margin zero

        -- fonts & text
        , textAlign left
        , Fonts.baseFont
        , fontSize (px 16)
        , fontWeight (int 600)
        , lineHeight (num 1.2)
        ]
    , headerExpandedStyles =
        [ color Colors.navy
        , borderRadius4 (px 8) (px 8) (px 0) (px 0)
        ]
    , headerClosedStyles =
        [ color Colors.azure
        , borderRadius (px 8)
        ]
    , contentStyles = [ overflow hidden ]
    }


{-| DefaultCaret is the blue caret
-}
type Caret
    = DefaultCaret
    | WhiteCaret
    | NoneCaret


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
    let
        resets =
            [ margin zero
            , padding zero
            ]
    in
    case headerLevel of
        H1 ->
            Accessibility.Styled.h1 [ Attributes.css resets ] [ content ]

        H2 ->
            Accessibility.Styled.h2 [ Attributes.css resets ] [ content ]

        H3 ->
            Accessibility.Styled.h3 [ Attributes.css resets ] [ content ]

        H4 ->
            Accessibility.Styled.h4 [ Attributes.css resets ] [ content ]

        H5 ->
            Accessibility.Styled.h5 [ Attributes.css resets ] [ content ]

        H6 ->
            Accessibility.Styled.h6 [ Attributes.css resets ] [ content ]


{-| -}
view :
    { entries : List { headerId : String, entry : entry, isExpanded : Bool }
    , headerLevel : HeaderLevel
    , viewHeader : entry -> Html msg
    , viewContent : entry -> Html msg
    , customStyles : Maybe (entry -> StyleOptions)
    , caret : Caret
    , toggle : entry -> Bool -> msg
    , focus : String -> msg
    }
    -> Html msg
view { entries, headerLevel, viewHeader, viewContent, customStyles, caret, toggle, focus } =
    let
        arrowUpIds : List (Maybe String)
        arrowUpIds =
            lastHeaderId :: List.map (.headerId >> Just) entries

        lastHeaderId : Maybe String
        lastHeaderId =
            Maybe.map .headerId (List.head (List.reverse entries))
    in
    div
        [ Attributes.class "accordion"
        , Attributes.attribute "aria-live" "polite"
        ]
        [ Html.Styled.Keyed.node "div"
            []
            (entries
                |> List.map2 (\id nextEntry -> ( id, nextEntry )) arrowUpIds
                |> List.foldr
                    (\( previousId, { headerId, entry, isExpanded } ) ( nextId, acc ) ->
                        let
                            node =
                                ( "keyed-section__" ++ headerId
                                , viewEntry
                                    { headerId = headerId
                                    , headerLevel = headerLevel
                                    , viewHeader = viewHeader
                                    , viewContent = viewContent
                                    , styleOptions = customStyles
                                    , caret = caret
                                    , toggle = toggle
                                    , arrowUp = Maybe.map focus previousId
                                    , arrowDown = Maybe.map focus nextId
                                    , entry = entry
                                    , isExpanded = isExpanded
                                    }
                                )
                        in
                        ( Just headerId
                        , node :: acc
                        )
                    )
                    ( Maybe.map .headerId (List.head entries), [] )
                |> Tuple.second
            )
        ]


viewEntry :
    { headerId : String
    , headerLevel : HeaderLevel
    , viewHeader : entry -> Html msg
    , viewContent : entry -> Html msg
    , styleOptions : Maybe (entry -> StyleOptions)
    , caret : Caret
    , toggle : entry -> Bool -> msg
    , arrowUp : Maybe msg
    , arrowDown : Maybe msg
    , entry : entry
    , isExpanded : Bool
    }
    -> Html msg
viewEntry { headerId, headerLevel, viewHeader, viewContent, styleOptions, caret, toggle, entry, isExpanded, arrowDown, arrowUp } =
    let
        newStyleOptions =
            case Maybe.map (\styles_ -> styles_ entry) styleOptions of
                Just { entryStyles, entryExpandedStyles, entryClosedStyles, headerStyles, headerExpandedStyles, headerClosedStyles, contentStyles } ->
                    { entryStyles = defaultStyleOptions.entryStyles ++ entryStyles
                    , entryExpandedStyles = defaultStyleOptions.entryExpandedStyles ++ entryExpandedStyles
                    , entryClosedStyles = defaultStyleOptions.entryClosedStyles ++ entryClosedStyles
                    , headerStyles = defaultStyleOptions.headerStyles ++ headerStyles
                    , headerExpandedStyles = defaultStyleOptions.headerExpandedStyles ++ headerExpandedStyles
                    , headerClosedStyles = defaultStyleOptions.headerClosedStyles ++ headerClosedStyles
                    , contentStyles = defaultStyleOptions.contentStyles ++ contentStyles
                    }

                Nothing ->
                    defaultStyleOptions

        styles =
            if isExpanded then
                { entry = newStyleOptions.entryStyles ++ newStyleOptions.entryExpandedStyles
                , header = newStyleOptions.headerStyles ++ newStyleOptions.headerExpandedStyles
                , content = newStyleOptions.contentStyles
                }

            else
                { entry = newStyleOptions.entryStyles ++ newStyleOptions.entryClosedStyles
                , header = newStyleOptions.headerStyles ++ newStyleOptions.headerClosedStyles
                , content = [ maxHeight (px 0) ]
                }

        panelId =
            "accordion-panel__" ++ headerId
    in
    section
        [ entryClass isExpanded
        , Attributes.css styles.entry
        ]
        [ header headerLevel <|
            button
                [ Attributes.id headerId
                , Attributes.class "accordion-entry-header"
                , Attributes.css styles.header
                , Widget.expanded isExpanded
                , Aria.controls panelId
                , onClick (toggle entry (not isExpanded))
                , Key.onKeyDown
                    (List.filterMap identity
                        [ Maybe.map Key.up arrowUp
                        , Maybe.map Key.down arrowDown
                        ]
                    )
                ]
                [ viewCaret isExpanded caret
                , viewHeader entry
                ]
        , div
            [ Attributes.id panelId
            , Attributes.class "accordion-entry-panel"
            , Attributes.hidden (not isExpanded)
            , Attributes.css styles.content
            ]
            [ viewContent entry ]
        ]


{-| Used for tests that rely on classnames, and couple edge cases where we need to override styles using sass
-}
entryClass : Bool -> Attribute msg
entryClass expanded =
    Attributes.classList
        [ ( "accordion-entry", True )
        , ( "accordion-entry-state-expanded", expanded )
        , ( "accordion-entry-state-collapsed", not expanded )
        ]


{-| Just the caret!
-}
viewCaret : Bool -> Caret -> Html msg
viewCaret expanded caret =
    case caret of
        DefaultCaret ->
            DisclosureIndicator.large
                [ marginRight (px 8)
                ]
                expanded

        WhiteCaret ->
            DisclosureIndicator.large
                [ marginRight (px 8)
                , Css.Global.descendants
                    [ Css.Global.everything [ color Colors.white ] ]
                ]
                expanded

        NoneCaret ->
            text ""
