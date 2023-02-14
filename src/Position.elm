module Position exposing (xOffsetPx, xOffsetPxAgainstContainer)

{-| -}

import Browser.Dom as Dom


{-| Figure out how much an element needs to shift along the horizontal axis in order to not be cut off by the viewport.

Uses Brower.Dom's Element measurement.

-}
xOffsetPx : Dom.Element -> Float
xOffsetPx { element, viewport } =
    let
        xMax =
            viewport.x + viewport.width
    in
    -- if the element is cut off by the viewport on the left side,
    -- we need to adjust rightward by the cut-off amount
    if element.x < viewport.x then
        viewport.x - element.x

    else
    -- if the element is cut off by the viewport on the right side,
    -- we need to adjust leftward by the cut-off amount
    if
        xMax < (element.x + element.width)
    then
        xMax - (element.x + element.width)

    else
        0


{-| Figure out how much an element needs to shift along the horizontal axis in order to not overflow its ancestor.

Uses Brower.Dom's Element measurement.

-}
xOffsetPxAgainstContainer : { container : Dom.Element, element : Dom.Element } -> Float
xOffsetPxAgainstContainer config =
    let
        container =
            config.container.element

        element =
            config.element.element

        xMax =
            container.x + container.width
    in
    -- if the element is cut off by the container on the left side,
    -- we need to adjust rightward by the cut-off amount
    if element.x < container.x then
        container.x - element.x

    else
    -- if the element is cut off by the container on the right side,
    -- we need to adjust leftward by the cut-off amount
    if
        xMax < (element.x + element.width)
    then
        xMax - (element.x + element.width)

    else
        0
