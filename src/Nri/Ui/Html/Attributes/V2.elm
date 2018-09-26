module Nri.Ui.Html.Attributes.V2 exposing (none, includeIf)

{-| Extras for working with Html.Attributes.

This is the new version of Nri.Ui.Html.Attributes.Extra.

@docs none, includeIf

-}

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes
import Json.Encode as Encode


{-| Represents an attribute with no semantic meaning, useful for conditionals.

This is implemented such that whenever Html.Attributes.Extra.none is encountered
by VirtualDom it will set a meaningless property on the element object itself to
null:

    domNode['Html.Attributes.Extra.none'] = null

It's totally safe and lets us clean up conditional and maybe attributes

-}
none : Attribute msg
none =
    Attributes.property "Html.Attributes.Extra.none" Encode.null


{-| conditionally include an attribute. Useful for CSS classes generated with
`UniqueClass`!
-}
includeIf : Bool -> Attribute msg -> Attribute msg
includeIf cond attr =
    if cond then
        attr

    else
        none
