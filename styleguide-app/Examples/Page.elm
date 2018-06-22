module Examples.Page exposing (example, styles)

{-|

@docs example, styles

-}

import Css
import Css.Foreign exposing (Snippet, adjacentSiblings, children, class, descendants, each, everything, media, selector, withClass)
import Headings
import Html exposing (Html)
import ModuleExample as ModuleExample exposing (Category(..), ModuleExample)
import Nri.Ui.Page.V1 as Page
import Nri.Ui.Styles.V1 as Styles


{-| -}
example : msg -> ModuleExample msg
example noOp =
    { filename = "Nri/Ui/Page/V1.elm"
    , category = Pages
    , content =
        [ Headings.h3 [ Html.text "Pages for problems" ]
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


{-| -}
styles : Styles.Styles id class msg
styles =
    Styles.styles "Examples-Page-"
        [ Css.Foreign.class Container
            [ Css.displayFlex
            , Css.flexWrap Css.wrap
            ]
        ]



-- INTERNAL


type CssClasses
    = Container
