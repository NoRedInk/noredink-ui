module Nri.Ui.Layout.V1 exposing
    ( centeredContent, content, narrowContent
    , pageSidePaddingPx, pageBottomMargin
    )

{-|

@docs centeredContent, content, narrowContent
@docs pageSidePaddingPx, pageBottomMargin

-}

import Css exposing (Style)
import Css.Media as Media
import Nri.Ui.MediaQuery.V1 as MediaQuery


{-| This is meant to be reusable for any area that:

  - should be centered
  - on wide viewports, should fill the width of the screen up to a max width of the mobile breakpoint
  - on narrow viewports, should have standard side padding

If you have a container that should snap flush to the edges on mobile, this isn't the right style to use.

-}
content : Style
content =
    Css.batch
        [ centeredContent
        , Media.withMedia [ MediaQuery.mobile ]
            [ Css.padding2 Css.zero pageSidePaddingPx
            ]
        ]


{-| Center content with a max width of the mobile breakpoint.

This style does not add side padding on mobile, which means that this can be used for containers that should snap flush to the edges of the mobile viewport.

-}
centeredContent : Style
centeredContent =
    Css.batch
        [ Css.maxWidth MediaQuery.mobileBreakpoint
        , Css.width (Css.pct 100)
        , Css.marginLeft Css.auto
        , Css.marginRight Css.auto
        ]


{-| Use this style on Quiz Engine pages.

This is identical to `content`, except that it uses the quizEngineMobile breakpoint instead of the mobile breakpoint.

If you have a container that should snap flush to the edges on mobile, this isn't the right style to use.

-}
narrowContent : Style
narrowContent =
    Css.batch
        [ narrowCenteredContent
        , Media.withMedia [ MediaQuery.quizEngineMobile ]
            [ Css.padding2 Css.zero pageSidePaddingPx
            ]
        ]


{-| Use this style on Quiz Engine pages.

This is identical to `centeredContent`, except that it uses the quizEngineMobile breakpoint instead of the mobile breakpoint.

Center content with a max width of the narrow breakpoint.

This style does not add side padding on mobile, which means that this can be used for containers that should snap flush to the edges of the mobile viewport.

-}
narrowCenteredContent : Style
narrowCenteredContent =
    Css.batch
        [ Css.maxWidth MediaQuery.quizEngineBreakpoint
        , Css.width (Css.pct 100)
        , Css.marginLeft Css.auto
        , Css.marginRight Css.auto
        ]


{-| Unless content is flush with the edges of the viewport, there should be 15px of left/right spacing between the content and the viewport edge.

See [the UI Style Guide and Caveats' Spacing section](https://paper.dropbox.com/doc/UI-Style-Guide-and-Caveats--BobQllelpdS56NBITiRcrO6gAg-PvOLxeX3oyujYEzdJx5pu#:uid=905917270049954035442315&h2=:under-construction:-Spacing) for more details.

-}
pageSidePaddingPx : Css.Px
pageSidePaddingPx =
    Css.px 15


{-| Convenience for adding the appriopriate amount of whitespace at the end of the page with margin.
-}
pageBottomMargin : Style
pageBottomMargin =
    Css.marginBottom pageBottomMarginPx


{-| Every page should have 50px of whitespace at the end, so that footers don't end up spanning the middle of the page, and for consistency's sake.

See [the UI Style Guide and Caveats' Spacing section](https://paper.dropbox.com/doc/UI-Style-Guide-and-Caveats--BobQllelpdS56NBITiRcrO6gAg-PvOLxeX3oyujYEzdJx5pu#:uid=905917270049954035442315&h2=:under-construction:-Spacing) for more details.

-}
pageBottomMarginPx : Css.Px
pageBottomMarginPx =
    Css.px 50
