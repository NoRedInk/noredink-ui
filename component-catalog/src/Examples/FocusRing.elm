module Examples.FocusRing exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Category exposing (Category(..))
import Css exposing (Style)
import Example exposing (Example)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes exposing (css)
import Nri.Ui.ClickableText.V3 as ClickableText
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.FocusRing.V1 as FocusRing
import Nri.Ui.Fonts.V1 as Fonts
import Nri.Ui.Heading.V3 as Heading
import Nri.Ui.Spacing.V1 as Spacing
import Nri.Ui.Table.V7 as Table


{-| -}
type alias State =
    ()


{-| -}
type alias Msg =
    ()


{-| -}
example : Example State Msg
example =
    { name = "FocusRing"
    , version = 1
    , categories = [ Text, Atoms ]
    , keyboardSupport = []
    , state = ()
    , update = \_ state -> ( state, Cmd.none )
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
        \_ _ ->
            [ Heading.h2
                [ Heading.plaintext "Examples"
                , Heading.css [ Css.marginTop Spacing.verticalSpacerPx ]
                ]
            , viewTable
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


exampleWithBorder : List Style -> Html msg
exampleWithBorder styles =
    example_
        (Css.border3 (Css.px 2) Css.dashed Colors.gray20
            :: styles
        )


viewTable : Html msg
viewTable =
    Table.view []
        [ Table.rowHeader
            { header = Html.text "Name"
            , view = \{ name } -> code [] [ text name ]
            , width = Css.pct 10
            , cellStyles = always [ Css.textAlign Css.left ]
            , sort = Nothing
            }
        , Table.custom
            { header = text "View"
            , view = .view
            , width = Css.pct 10
            , cellStyles = always []
            , sort = Nothing
            }
        , Table.string
            { header = "About"
            , value = .about
            , width = Css.pct 10
            , cellStyles = \_ -> [ Fonts.baseFont, Css.padding (Css.px 8) ]
            , sort = Nothing
            }
        ]
        [ { name = "styles"
          , view = exampleWithBorder FocusRing.styles
          , about =
                """
A two-tone focus ring that will be visually apparent for any background/element combination.

NOTE: use `boxShadows` instead if your focusable element:

  - already has a box shadow
  - has an explicit border radius set
"""
          }
        , { name = "tightStyles"
          , view = exampleWithBorder FocusRing.tightStyles
          , about = "Prefer `styles` over tightStyles, except in cases where line spacing/font size will otherwise cause obscured content."
          }
        , { name = "boxShadows"
          , view = exampleWithBorder [ FocusRing.boxShadows [] ]
          , about = ""
          }
        , { name = "insetBoxShadows"
          , view = exampleWithBorder [ FocusRing.insetBoxShadows [] ]
          , about = "Please be sure that the padding on the element you add this style too is sufficient (at least 6px on all sides) that the inset box shadow won't cover any content."
          }
        , { name = "outerBoxShadow"
          , view = exampleWithBorder [ FocusRing.outerBoxShadow ]
          , about = "In special cases, we don't use a two-tone focus ring. Be very sure this is what you need before using this!"
          }
        , { name = "insetBoxShadow"
          , view = exampleWithBorder [ FocusRing.insetBoxShadow ]
          , about = "In special cases, we don't use a two-tone focus ring, and an outset focus ring would be obscured. Be very sure this is what you need before using this!"
          }
        , { name = "outerColor"
          , view = exampleWithBorder [ Css.backgroundColor FocusRing.outerColor ]
          , about = "Colors.red"
          }
        , { name = "innerColor"
          , view = exampleWithBorder [ Css.backgroundColor FocusRing.innerColor ]
          , about = "Colors.white"
          }
        ]
