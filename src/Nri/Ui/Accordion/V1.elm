module Nri.Ui.Accordion.V1 exposing
    ( view, viewKeyed
    , viewCaret, Caret(..)
    , AccordionOptions, StyleOptions
    )

{-|

@docs view, viewKeyed
@docs viewCaret, Caret
@docs AccordionOptions, StyleOptions

-}

import Accessibility.Styled exposing (Attribute, Html, button, div, text)
import Css exposing (..)
import Css.Global
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onClick)
import Html.Styled.Keyed
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.DisclosureIndicator.V2 as DisclosureIndicator
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Svg.V1 as Svg


{-| -}
type alias AccordionOptions entry msg =
    { entries : List ( entry, Bool )
    , viewHeader : entry -> Html msg
    , viewContent : entry -> Html msg
    , customStyles : Maybe (entry -> StyleOptions)
    , caret : Caret
    , toggle : entry -> Bool -> msg
    }


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
view : AccordionOptions entry msg -> Html msg
view { entries, viewHeader, viewContent, customStyles, caret, toggle } =
    div
        [ Attributes.class "accordion"
        , Attributes.attribute "role" "tablist"
        , Attributes.attribute "aria-live" "polite"
        ]
        (List.map (viewEntry viewHeader viewContent customStyles caret toggle) entries)


{-| If your accordion's rows can be moved around, use viewKeyed. It prevents
the caret's animation from firing off incorrectly when rows move.
-}
viewKeyed : AccordionOptions entry msg -> (entry -> String) -> Html msg
viewKeyed { entries, viewHeader, viewContent, customStyles, caret, toggle } identifier =
    div
        [ Attributes.class "accordion"
        , Attributes.attribute "role" "tablist"
        , Attributes.attribute "aria-live" "polite"
        ]
        [ Html.Styled.Keyed.node "div"
            []
            (List.map
                (\( entry, isExpanded ) ->
                    ( identifier entry
                    , viewEntry viewHeader viewContent customStyles caret toggle ( entry, isExpanded )
                    )
                )
                entries
            )
        ]


viewEntry :
    (entry -> Html msg)
    -> (entry -> Html msg)
    -> Maybe (entry -> StyleOptions)
    -> Caret
    -> (entry -> Bool -> msg)
    -> ( entry, Bool )
    -> Html msg
viewEntry viewHeader viewContent styleOptions caret toggle ( entry, expanded ) =
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
            if expanded then
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
        [ entryClass expanded
        , Attributes.attribute "role" "tab"
        , Attributes.css styles.entry
        ]
        [ entryHeader expanded viewHeader styles.header caret toggle entry
        , entryPanel expanded viewContent styles.content entry
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


entryHeader :
    Bool
    -> (entry -> Html msg)
    -> List Style
    -> Caret
    -> (entry -> Bool -> msg)
    -> entry
    -> Html msg
entryHeader expanded viewHeader styles caret toggle entry =
    button
        [ Attributes.class "accordion-entry-header"
        , onClick (toggle entry (not expanded))
        , Attributes.css styles
        ]
        [ viewCaret expanded caret
        , viewHeader entry
        ]



-- TODO change content/body instances to panel


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
                |> Svg.toHtml

        WhiteCaret ->
            DisclosureIndicator.large
                [ marginRight (px 8)
                , Css.Global.descendants
                    [ Css.Global.everything [ color Colors.white ] ]
                ]
                expanded
                |> Svg.toHtml

        NoneCaret ->
            text ""
