module Nri.Ui.Spacing.V1 exposing
    ( centeredContentWithSidePadding, centeredContent
    , centeredQuizEngineContentWithSidePadding, quizEngineCenteredContent
    , centeredContentWithSidePaddingAndCustomWidth, centeredContentWithCustomWidth
    , pageTopWhitespace, pageTopWhitespacePx
    , pageSideWhitespace, pageSideWhitespacePx
    , pageBottomWhitespace, pageBottomWhitespacePx
    , verticalSpacerPx, horizontalSpacerPx
    )

{-|


## Center a container on the page:

@docs centeredContentWithSidePadding, centeredContent
@docs centeredQuizEngineContentWithSidePadding, quizEngineCenteredContent
@docs centeredContentWithSidePaddingAndCustomWidth, centeredContentWithCustomWidth


## Whitespace constants and helpers:

@docs pageTopWhitespace, pageTopWhitespacePx
@docs pageSideWhitespace, pageSideWhitespacePx
@docs pageBottomWhitespace, pageBottomWhitespacePx
@docs verticalSpacerPx, horizontalSpacerPx

-}

import Css exposing (Style)
import Css.Media as Media
import Nri.Ui.MediaQuery.V1 as MediaQuery


{-| Advanced use only: center content up to a custom page width, with side padding when narrower.
-}
centeredContentWithSidePaddingAndCustomWidth : Css.Px -> Style
centeredContentWithSidePaddingAndCustomWidth breakpoint =
    Css.batch
        [ centeredContentWithCustomWidth breakpoint
        , -- this media query is narrower to prevent the page from "snapping" into the
          -- narrow viewport when resizing. Visual shifts should be as minimal as possible
          -- when resizing.
          Media.withMediaQuery
            [ "screen and (max-width: "
                ++ .value
                    (Css.calc breakpoint
                        Css.minus
                        (Css.calc pageSideWhitespacePx Css.plus pageSideWhitespacePx)
                    )
                ++ ")"
            ]
            [ pageSideWhitespace ]
        ]


{-| Advanced use only: center content up to a custom page width.
-}
centeredContentWithCustomWidth : Css.Px -> Style
centeredContentWithCustomWidth maxWidth =
    Css.batch
        [ Css.maxWidth maxWidth
        , Css.width (Css.pct 100)
        , Css.marginLeft Css.auto
        , Css.marginRight Css.auto
        ]


{-| This is meant to be reusable for any area that:

  - should be centered
  - on wide viewports, should fill the width of the screen up to a max width of the mobile breakpoint
  - on narrow viewports, should have standard side padding

If you have a container that should snap flush to the edges on mobile, this isn't the right style to use.

-}
centeredContentWithSidePadding : Style
centeredContentWithSidePadding =
    centeredContentWithSidePaddingAndCustomWidth MediaQuery.mobileBreakpoint


{-| Center content with a max width of the mobile breakpoint.

This style does not add side padding on mobile, which means that this can be used for containers that should snap flush to the edges of the mobile viewport.

-}
centeredContent : Style
centeredContent =
    centeredContentWithCustomWidth MediaQuery.mobileBreakpoint


{-| Use this style on Quiz Engine pages.

This is identical to `content`, except that it uses the quizEngineMobile breakpoint instead of the mobile breakpoint.

If you have a container that should snap flush to the edges on mobile, this isn't the right style to use.

-}
centeredQuizEngineContentWithSidePadding : Style
centeredQuizEngineContentWithSidePadding =
    centeredContentWithSidePaddingAndCustomWidth MediaQuery.quizEngineBreakpoint


{-| Use this style on Quiz Engine pages.

This is identical to `centeredContent`, except that it uses the quizEngineMobile breakpoint instead of the mobile breakpoint.

This style does not add side padding on mobile, which means that this can be used for containers that should snap flush to the edges of the mobile viewport.

-}
quizEngineCenteredContent : Style
quizEngineCenteredContent =
    centeredContentWithCustomWidth MediaQuery.quizEngineBreakpoint


{-| Convenience for adding the appriopriate amount of whitespace on the sides of a full-width container on the page or on the page with side padding.
-}
pageSideWhitespace : Style
pageSideWhitespace =
    Css.batch
        [ Css.paddingLeft pageSideWhitespacePx
        , Css.paddingRight pageSideWhitespacePx
        ]


{-| Unless content is flush with the edges of the viewport, there should be 15px of left/right spacing between the content and the viewport edge.

See [the UI Style Guide and Caveats' Spacing section](https://paper.dropbox.com/doc/UI-Style-Guide-and-Caveats--BobQllelpdS56NBITiRcrO6gAg-PvOLxeX3oyujYEzdJx5pu#:uid=905917270049954035442315&h2=:under-construction:-Spacing) for more details.

-}
pageSideWhitespacePx : Css.Px
pageSideWhitespacePx =
    Css.px 15


{-| Convenience for adding the appriopriate amount of whitespace at the end of the page with margin.
-}
pageBottomWhitespace : Style
pageBottomWhitespace =
    Css.marginBottom pageBottomWhitespacePx


{-| Every page should have 50px of whitespace at the end, so that footers don't end up spanning the middle of the page, and for consistency's sake.

See [the UI Style Guide and Caveats' Spacing section](https://paper.dropbox.com/doc/UI-Style-Guide-and-Caveats--BobQllelpdS56NBITiRcrO6gAg-PvOLxeX3oyujYEzdJx5pu#:uid=905917270049954035442315&h2=:under-construction:-Spacing) for more details.

-}
pageBottomWhitespacePx : Css.Px
pageBottomWhitespacePx =
    Css.px 50


{-| Convenience for adding the appriopriate amount of whitespace at the end of the page with margin.
-}
pageTopWhitespace : Style
pageTopWhitespace =
    Css.marginTop pageTopWhitespacePx


{-| Every page should have 30px of whitespace separating the header nav and the page content, as well as before any secondary headers.

See [the UI Style Guide and Caveats' Spacing section](https://paper.dropbox.com/doc/UI-Style-Guide-and-Caveats--BobQllelpdS56NBITiRcrO6gAg-PvOLxeX3oyujYEzdJx5pu#:uid=905917270049954035442315&h2=:under-construction:-Spacing) for more details.

-}
pageTopWhitespacePx : Css.Px
pageTopWhitespacePx =
    Css.px 30


{-| Most elements should have 20px of whitespace separating them vertically.

See [the UI Style Guide and Caveats' Spacing section](https://paper.dropbox.com/doc/UI-Style-Guide-and-Caveats--BobQllelpdS56NBITiRcrO6gAg-PvOLxeX3oyujYEzdJx5pu#:uid=905917270049954035442315&h2=:under-construction:-Spacing) for more details.

-}
verticalSpacerPx : Css.Px
verticalSpacerPx =
    Css.px 20


{-| Most elements should have 10px of whitespace separating them horizontally.

See [the UI Style Guide and Caveats' Spacing section](https://paper.dropbox.com/doc/UI-Style-Guide-and-Caveats--BobQllelpdS56NBITiRcrO6gAg-PvOLxeX3oyujYEzdJx5pu#:uid=905917270049954035442315&h2=:under-construction:-Spacing) for more details.

-}
horizontalSpacerPx : Css.Px
horizontalSpacerPx =
    Css.px 10
