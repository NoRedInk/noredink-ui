module Nri.Ui.Layout.V1 exposing
    ( centeredContent, content, narrowContent
    , sidePaddingPx, bottomMargin
    )

{-|

@docs centeredContent, content, narrowContent
@docs sidePaddingPx, bottomMargin

-}

import Css exposing (Style)
import Css.Media as Media
import Nri.Ui.MediaQuery.V1 as MediaQuery


{-| This is meant to be reusable for any area that should fill the width of the screen
and apply the normal max-width to its content.
You can provide additional CSS styles (typically background or border, or flexbox styles).

This is a responsive container, and will automatically add left and right padding when the
screen gets too small.

-}
content : Style
content =
    Css.batch
        [ centeredContent
        , Media.withMedia [ MediaQuery.mobile ]
            [ Css.padding2 Css.zero sidePaddingPx
            ]
        ]


{-| Center content with a max width of the mobile breakpoint.
-}
centeredContent : Style
centeredContent =
    Css.batch
        [ Css.maxWidth MediaQuery.mobileBreakpoint
        , Css.width (Css.pct 100)
        , Css.marginLeft Css.auto
        , Css.marginRight Css.auto
        ]


{-| A narrower version of `content`
-}
narrowContent : Style
narrowContent =
    Css.batch
        [ narrowCenteredContent
        , Media.withMedia [ MediaQuery.quizEngineMobile ]
            [ Css.padding2 Css.zero sidePaddingPx
            ]
        ]


{-| Center content with a max width of the narrow breakpoint.
-}
narrowCenteredContent : Style
narrowCenteredContent =
    Css.batch
        [ Css.maxWidth MediaQuery.quizEngineBreakpoint
        , Css.width (Css.pct 100)
        , Css.marginLeft Css.auto
        , Css.marginRight Css.auto
        ]


sidePaddingPx : Css.Px
sidePaddingPx =
    Css.px 15


{-| See <https://paper.dropbox.com/doc/UI-Style-Guide-and-Caveats--BnyI9dz1Cs5CnP2IOc276_fQAg-PvOLxeX3oyujYEzdJx5pu>
-}
bottomMargin : Style
bottomMargin =
    Css.marginBottom (Css.px 50)
