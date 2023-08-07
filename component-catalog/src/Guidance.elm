module Guidance exposing (..)

import Html.Styled exposing (..)
import Nri.Ui.ClickableText.V3 as ClickableText


useATACGuide : List (Html msg)
useATACGuide =
    [ text "To ensure your use of this component is accessible to assistive technology, please review the "
    , ClickableText.link "Assistive technology notification design & development guide"
        [ ClickableText.linkExternal "https://noredinkaccessibility.screenstepslive.com/a/1651037-assistive-technology-notification-design-development-guide"
        ]
    , text " to see if your use case fits any listed in the guide. If it does, please follow the guide to learn how to properly implement this component."
    ]
