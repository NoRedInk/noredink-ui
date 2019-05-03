module Examples.Page exposing (example)

{-|

@docs example, styles

-}

import Css
import Css.Global exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Headings
import Html.Styled as Html exposing (Html)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Page.V2 as Page


{-| -}
example : msg -> ModuleExample msg
example noOp =
    { filename = "Nri/Ui/Page/V1.elm"
    , category = Pages
    , content =
        [ Css.Global.global
            [ Css.Global.selector "[data-page-container]"
                [ Css.displayFlex
                , Css.flexWrap Css.wrap
                ]
            ]
        , Headings.h4 [ Html.text "Page: Not Found" ]
        , Page.notFound
            { link = noOp
            , name = "The Main Page"
            }
        , Headings.h4 [ Html.text "Page: Broken" ]
        , Page.broken
            { link = noOp
            , name = "The Main Page"
            }
        , Headings.h4 [ Html.text "Page: No Permission" ]
        , Page.noPermission
            { link = noOp
            , name = "The Main Page"
            }
        ]
    }
