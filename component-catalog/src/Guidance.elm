module Guidance exposing (..)

import Examples.RadioButtonDotless as RadioButtonDotlessExample
import Html.Styled exposing (..)
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Message.V4 as Message
import Nri.Ui.Text.V6 as Text
import Routes


useATACGuide : String -> List (Html msg)
useATACGuide moduleName =
    [ Text.smallBody
        [ Text.html
            [ text ("To ensure your use of " ++ moduleName ++ " is accessible to assistive technology, please review the ")
            , ClickableText.link "Assistive technology notification design & development guide"
                [ ClickableText.linkExternal "https://noredinkaccessibility.screenstepslive.com/a/1651037-assistive-technology-notification-design-development-guide"
                , ClickableText.appearsInline
                ]
            , text (" to see if your use case fits any listed in the guide. If it does, please follow the guide to learn how to properly implement " ++ moduleName ++ ".")
            ]
        ]
    ]


useRadioButtonDotless : Html msg
useRadioButtonDotless =
    Message.view
        [ Message.html
            [ text "Looking for a group of buttons where only one button is selectable at a time? Check out "
            , ClickableText.link "RadioButtonDotless"
                [ ClickableText.href (Routes.exampleHref RadioButtonDotlessExample.example)
                , ClickableText.appearsInline
                ]
            , text "."
            ]
        ]


helpfullyDisabled : String -> Html msg
helpfullyDisabled moduleName =
    Text.smallBody
        [ Text.html
            [ text ("Is your " ++ moduleName ++ " sometimes disabled? Be sure to ")
            , ClickableText.link "read the docs"
                [ ClickableText.linkExternal "https://paper.dropbox.com/doc/Helpfully-disabled-components--CI8Ma_KHKL1CcCWpWG~p_RTwAg-2RUPgKnBsBNI7ScGDHS73"
                , ClickableText.appearsInline
                ]
            , text " and "
            , ClickableText.link "watch Charbel's demo"
                [ ClickableText.linkExternal "https://noredink.zoom.us/rec/play/fwV3mqsxjvF_95N2au0vAN2PmnH2IHZx2yCoAQ76gvZ0fLlrkNcFIuVL6i7ze7y1ivSxq0f6e2EXE-RJ.kHMKX9CBHI1kFM50?canPlayFromShare=true&from=share_recording_detail&continueMode=true&componentName=rec-play&originRequestUrl=https://noredink.zoom.us/rec/share/YvgK0427ADw42fY2edJ_tmkwwvPxz505Kpfhkz5DqF1_eh8sgj7wVfwBQ5FmieM8.P9YlMkM_XY_Kamm6&autoplay=true&startTime=1696520905000&_x_zm_rtaid=VeLjvOzDToKMf1R0XllC7A.1707171050117.67806369f8182aa5b282c10165d75544&_x_zm_rhtaid=323"
                , ClickableText.appearsInline
                ]
            , text " on the Helpfully Disabled pattern."
            ]
        ]
