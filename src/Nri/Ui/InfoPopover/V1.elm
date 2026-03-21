module Nri.Ui.InfoPopover.V1 exposing
    ( view
    , Attribute
    , plaintext, paragraph, markdown, html
    , withoutTail
    , onTop, onBottom, onLeft, onRight
    , onTopForMobile, onBottomForMobile, onLeftForMobile, onRightForMobile
    , alignStart, alignMiddle, alignEnd
    , exactWidth, fitToContent
    , smallPadding, normalPadding, customPadding
    , open, onToggle
    , css, testId, nriDescription
    )

{-| Overflow-safe disclosure content for rich, interactive popovers.

Use this module when the revealed content behaves more like a disclosure or
small dialog than a passive tooltip.

@docs view
@docs Attribute
@docs plaintext, paragraph, markdown, html
@docs withoutTail
@docs onTop, onBottom, onLeft, onRight
@docs onTopForMobile, onBottomForMobile, onLeftForMobile, onRightForMobile
@docs alignStart, alignMiddle, alignEnd
@docs exactWidth, fitToContent
@docs smallPadding, normalPadding, customPadding
@docs open, onToggle
@docs css, testId, nriDescription

-}

import Accessibility.Styled as Html exposing (Html)
import Accessibility.Styled.Key as Key
import Css
import EventExtras
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode as Decode
import Nri.Ui.AnchoredOverlay.Internal.V1 as Internal


{-| Configuration attributes for `InfoPopover.view`.
-}
type alias Attribute msg =
    Internal.Attribute msg


{-| Render a controlled info popover anchored to a trigger.
-}
view :
    { id : String
    , triggerId : String
    , label : String
    , trigger : List (Html.Attribute msg) -> Html msg
    }
    -> List (Attribute msg)
    -> Html msg
view { id, triggerId, label, trigger } attributes =
    let
        overlay =
            Internal.buildAttributes attributes
    in
    Internal.view
        { id = id
        , anchorId = triggerId
        , trigger = trigger
        , mode = Internal.Popover
        , wrapperDescription = "Nri-Ui-InfoPopover-V1"
        , wrapperEvents = []
        , triggerAttributes =
            \config ->
                [ Attributes.id triggerId
                , Attributes.attribute "aria-haspopup" "dialog"
                , Attributes.attribute "aria-expanded"
                    (if config.isOpen then
                        "true"

                     else
                        "false"
                    )
                , Attributes.attribute "aria-controls" id
                ]
                    ++ toggleEvents config
        , hostAttributes =
            \config ->
                [ Attributes.attribute "role" "dialog"
                , Attributes.attribute "aria-label" label
                ]
                    ++ maybeRequestClose config
        }
        overlay


toggleEvents : Internal.Config msg -> List (Html.Attribute msg)
toggleEvents config =
    case config.onToggle of
        Just toMsg ->
            [ EventExtras.onClickPreventDefaultAndStopPropagation (toMsg (not config.isOpen))
            , Key.onKeyDownPreventDefault
                [ Key.enter (toMsg (not config.isOpen))
                , Key.space (toMsg (not config.isOpen))
                ]
            ]

        Nothing ->
            []


maybeRequestClose : Internal.Config msg -> List (Html.Attribute msg)
maybeRequestClose config =
    case config.onToggle of
        Just toMsg ->
            [ Events.on "request-close" (Decode.succeed (toMsg False)) ]

        Nothing ->
            []


{-| Provide plain text content.
-}
plaintext : String -> Attribute msg
plaintext =
    Internal.plaintext


{-| Provide plain text content wrapped in a paragraph.
-}
paragraph : String -> Attribute msg
paragraph =
    Internal.paragraph


{-| Provide inline markdown content.
-}
markdown : String -> Attribute msg
markdown =
    Internal.markdown


{-| Provide custom HTML content.
-}
html : List (Html msg) -> Attribute msg
html =
    Internal.html


{-| Remove the popover tail.
-}
withoutTail : Attribute msg
withoutTail =
    Internal.withoutTail


{-| Prefer placing the popover above the trigger.
-}
onTop : Attribute msg
onTop =
    Internal.onTop


{-| Prefer placing the popover below the trigger.
-}
onBottom : Attribute msg
onBottom =
    Internal.onBottom


{-| Prefer placing the popover to the left of the trigger.
-}
onLeft : Attribute msg
onLeft =
    Internal.onLeft


{-| Prefer placing the popover to the right of the trigger.
-}
onRight : Attribute msg
onRight =
    Internal.onRight


{-| Use top placement on mobile.
-}
onTopForMobile : Attribute msg
onTopForMobile =
    Internal.onTopForMobile


{-| Use bottom placement on mobile.
-}
onBottomForMobile : Attribute msg
onBottomForMobile =
    Internal.onBottomForMobile


{-| Use left placement on mobile.
-}
onLeftForMobile : Attribute msg
onLeftForMobile =
    Internal.onLeftForMobile


{-| Use right placement on mobile.
-}
onRightForMobile : Attribute msg
onRightForMobile =
    Internal.onRightForMobile


{-| Align the popover toward the start of the trigger.
-}
alignStart : Attribute msg
alignStart =
    Internal.alignStart


{-| Center-align the popover against the trigger.
-}
alignMiddle : Attribute msg
alignMiddle =
    Internal.alignMiddle


{-| Align the popover toward the end of the trigger.
-}
alignEnd : Attribute msg
alignEnd =
    Internal.alignEnd


{-| Set an exact popover width in pixels.
-}
exactWidth : Int -> Attribute msg
exactWidth =
    Internal.exactWidth


{-| Size the popover to its content.
-}
fitToContent : Attribute msg
fitToContent =
    Internal.fitToContent


{-| Use the compact padding preset.
-}
smallPadding : Attribute msg
smallPadding =
    Internal.smallPadding


{-| Use the default padding preset.
-}
normalPadding : Attribute msg
normalPadding =
    Internal.normalPadding


{-| Set custom padding in pixels.
-}
customPadding : Float -> Attribute msg
customPadding =
    Internal.customPadding


{-| Control whether the popover is open.
-}
open : Bool -> Attribute msg
open =
    Internal.open


{-| Receive open and close requests from trigger clicks, outside interactions, and Escape handling.
-}
onToggle : (Bool -> msg) -> Attribute msg
onToggle =
    Internal.onToggle


{-| Extend the popover bubble styles.
-}
css : List Css.Style -> Attribute msg
css =
    Internal.css


{-| Add a `data-testid` to the popover bubble.
-}
testId : String -> Attribute msg
testId =
    Internal.testId


{-| Add a `data-nri-description` to the popover bubble.
-}
nriDescription : String -> Attribute msg
nriDescription =
    Internal.nriDescription
