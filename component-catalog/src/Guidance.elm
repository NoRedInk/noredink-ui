module Guidance exposing (..)

import Examples.RadioButtonDotless as RadioButtonDotlessExample
import Html.Styled exposing (..)
import Nri.Ui.ClickableText.V4 as ClickableText
import Nri.Ui.Message.V4 as Message
import Nri.Ui.Text.V6 as Text
import Routes


useATACGuide : String -> List (Html msg)
useATACGuide moduleName =
    [ Text.mediumBody
        [ Text.html
            [ text ("To ensure your use of " ++ moduleName ++ " is accessible to assistive technology, please review the ")
            , ClickableText.link "Assistive technology notification design & development guide"
                [ ClickableText.linkExternal "https://noredinkaccessibility.screenstepslive.com/a/1651037-assistive-technology-notification-design-development-guide"
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
            ]
        ]
