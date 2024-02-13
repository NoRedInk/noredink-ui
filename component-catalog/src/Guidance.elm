module Guidance exposing (..)

import Examples.RadioButtonDotless as RadioButtonDotlessExample
import Html.Styled exposing (..)
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Message.V4 as Message
import Nri.Ui.Text.V6 as Text
import Routes


communicateState : String -> List (Html msg)
communicateState moduleName =
    [ Text.smallBody
        [ Text.html
            [ text "As long as this component’s semantics follow accessibility guidelines and best practices, this component’s state and other meaningful attributes should be communicated automatically to screen readers & similar assistive technology without the need to send special announcements to the "
            , ClickableText.link "ATAC"
                [ ClickableText.linkExternal "https://paper.dropbox.com/doc/Assistive-Technology-Announcement-Center-ATAC-bOnmcnzOj631NRls1IBe3"
                , ClickableText.appearsInline
                ]
            , text ". "
            , ClickableText.link "Learn more about how state is communicated automatically"
                [ ClickableText.linkExternal "https://noredinkaccessibility.screenstepslive.com/a/1692512-communicating-state"
                , ClickableText.appearsInline
                ]
            , text ". "
            ]
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


message : String -> Html msg
message moduleName =
    Text.smallBody
        [ Text.html
            [ text "Watch "
            , ClickableText.link "Tessa’s demo"
                [ ClickableText.linkExternal "https://noredink.zoom.us/rec/play/04Yg5m66aiM4gzzeOUFxWDkCin0Z6_zXwb5Jq3R7xqSgSIqWZTL_xNCDmnUpromV4bupQrx92JMjlKTi.ubiUciVbg1C4Y5eN?canPlayFromShare=true&from=share_recording_detail&startTime=1686841407000&componentName=rec-play&originRequestUrl=https%3A%2F%2Fnoredink.zoom.us%2Frec%2Fshare%2FBN88Pbq50Maz2A3oZgSC0cSzKPwFFPBxzCKJs3rxi_MlcuWYCyUNLIEIMybujrIN.qjwOXXxEAyVdoED4%3FstartTime%3D1686841407000%26_x_zm_rtaid%3DexJoa-qAQhSpO_3ZAnlvOg.1686929322690.d6880919fcdc54dce47ba87f206842e0%26_x_zm_rhtaid%3D822"
                , ClickableText.appearsInline
                ]
            , text " to learn more about the Message component and message helpers like "
            , inlineCode "errorMessage"
            , text " and "
            , inlineCode "guidance"
            , text " on form elements."
            ]
        ]


useATACGuide : String -> List (Html msg)
useATACGuide moduleName =
    [ Text.mediumBody
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


inlineCode : String -> Html msg
inlineCode text_ =
    code [] [ text text_ ]
