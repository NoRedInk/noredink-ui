module Nri.Ui.MediaQuery.V1 exposing
    ( mobile, notMobile
    , teacherStudentPage
    )

{-| Standard media queries for NRI responsive pages.
Can be used to create a Snippet using Css.Global.media

@docs mobile, notMobile
@docs teacherStudentPage

-}

import Css exposing (px)
import Css.Media exposing (MediaQuery, maxWidth, minWidth, only, screen)


{-| Mobile styles using a 1000px max-width

`minWidth (px 1)` is for a bug in IE which causes the media query to initially trigger regardless of window size

See: <http://stackoverflow.com/questions/25673707/ie11-triggers-css-transition-on-page-load-when-non-applied-media-query-exists/25850649#25850649>

-}
mobile : MediaQuery
mobile =
    only screen
        [ minWidth (px 1)
        , maxWidth (px 1000)
        ]


{-| Non-mobile styles using a 1000px min-width
-}
notMobile : MediaQuery
notMobile =
    only screen [ minWidth (px 1000) ]


{-| Teacher & student facing styles using a 700px max-width

`minWidth (px 1)` is for a bug in IE which causes the media query to initially trigger regardless of window size

See: <http://stackoverflow.com/questions/25673707/ie11-triggers-css-transition-on-page-load-when-non-applied-media-query-exists/25850649#25850649>

-}
teacherStudentPage : MediaQuery
teacherStudentPage =
    only screen
        [ minWidth (px 1)
        , maxWidth (px 700)
        ]
