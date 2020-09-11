module Nri.Ui.Accordion.V2 exposing (view, Caret(..), StyleOptions)

{-| Changes from V1:

  - Combine view and viewKeyed so that nodes are always keyed
  - Removes viewCaret from the API -- it's possible to use the DisclosureIndicator directly
  - Changed implementation to follow recommendations from <https://www.w3.org/TR/wai-aria-practices-1.1/examples/accordion/accordion.html>

@docs view, Caret, StyleOptions

-}

import Accessibility.Styled exposing (Attribute, Html, button, div, text)
import Accessibility.Styled.Key as Key
import Accessibility.Styled.Role as Role
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


{-| -}
view :
    { entries : List { headerId : String, entry : entry, isExpanded : Bool }
    , viewHeader : entry -> Html msg
    , viewContent : entry -> Html msg
    , customStyles : Maybe (entry -> StyleOptions)
    , caret : Caret
    , toggle : entry -> Bool -> msg
    , focus : String -> msg
    }
    -> Html msg
view { entries, viewHeader, viewContent, customStyles, caret, toggle, focus } =
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
        , Attributes.attribute "role" "tablist"
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
viewEntry { headerId, viewHeader, viewContent, styleOptions, caret, toggle, entry, isExpanded, arrowDown, arrowUp } =
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
    in
    div
        [ entryClass isExpanded
        , Attributes.attribute "role" "tab"
        , Attributes.css styles.entry
        ]
        [ button
            [ Attributes.id headerId
            , Attributes.class "accordion-entry-header"
            , Attributes.css styles.header
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
        , entryPanel isExpanded viewContent styles.content entry
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


entryPanel : Bool -> (entry -> Html msg) -> List Style -> entry -> Html msg
entryPanel expanded viewContent styles entry =
    div
        [ Attributes.class "accordion-entry-panel"
        , Attributes.hidden (not expanded)
        , Attributes.attribute "role" "tabpanel"
        , Attributes.css styles
        ]
        [ viewContent entry ]


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
