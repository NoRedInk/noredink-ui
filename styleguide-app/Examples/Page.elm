module Examples.Page exposing (example)

{-|

@docs example, styles

-}

import Category exposing (Category(..))
import Css
import Css.Global exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Html.Styled as Html exposing (Html)
import ModuleExample exposing (ModuleExample)
import Nri.Ui.Heading.V2 as Heading
import Nri.Ui.Page.V3 as Page


{-| -}
example : msg -> ModuleExample msg
example noOp =
    { name = "Nri.Ui.Page.V3"
    , category = Pages
    , content =
        [ Css.Global.global
            [ Css.Global.selector "[data-page-container]"
                [ Css.displayFlex
                , Css.flexWrap Css.wrap
                ]
            ]
        , Heading.h3 [] [ Html.text "Page: Not Found, recovery text: ReturnTo" ]
        , Page.notFound
            { link = noOp
            , recoveryText = Page.ReturnTo "the main page"
            }
        , Heading.h3 [] [ Html.text "Page: Broken, recovery text: Reload" ]
        , Page.broken
            { link = noOp
            , recoveryText = Page.Reload
            }
        , Heading.h3 [] [ Html.text "Page: No Permission, recovery text: Custom" ]
        , Page.noPermission
            { link = noOp
            , recoveryText = Page.Custom "Hit the road, Jack"
            }
        ]
    }
