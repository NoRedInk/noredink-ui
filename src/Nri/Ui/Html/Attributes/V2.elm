module Nri.Ui.Html.Attributes.V2 exposing
    ( none, includeIf, maybe
    , targetBlank
    , nriDescription, nriDescriptionSelector
    , testId
    )

{-|


# Patch changes:

    - adds `nriDescription` and `testId` helpers
    - adds `maybe` helper

Extras for working with Html.Attributes.

This is the new version of Nri.Ui.Html.Attributes.Extra.

@docs none, includeIf, maybe
@docs targetBlank
@docs nriDescription, nriDescriptionSelector
@docs testId

-}

import Css
import Css.Global
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


{-| Transform a maybe value to an attribute or attach `none`
-}
maybe : (v -> Attribute msg) -> Maybe v -> Attribute msg
maybe toAttr =
    Maybe.map toAttr >> Maybe.withDefault none


{-| conditionally include an attribute. Useful for CSS classes generated with
`UniqueClass`!
-}
includeIf : Bool -> Attribute msg -> Attribute msg
includeIf cond attr =
    if cond then
        attr

    else
        none


{-| Use this list of attributes instead of applying `Attributes.target "_blank"`
directly. This prevents an exploits like "tabnabbing", among other things.

See these resources:

  - <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a#Security_and_privacy_concerns>
  - <https://www.jitbit.com/alexblog/256-targetblank---the-most-underestimated-vulnerability-ever>

-}
targetBlank : List (Attribute msg)
targetBlank =
    [ Attributes.target "_blank"
    , Attributes.rel "noopener noreferrer"
    ]


{-| See Cypress best practices: <https://docs.cypress.io/guides/references/best-practices.html#Selecting-Elements>
-}
testId : String -> Attribute msg
testId id =
    Attributes.attribute "data-testid" id


{-| -}
nriDescription : String -> Attribute msg
nriDescription description =
    Attributes.attribute "data-nri-description" description


{-|

    Note: this does not handle html escaping the description before building the query

-}
nriDescriptionSelector : String -> List Css.Style -> Css.Global.Snippet
nriDescriptionSelector description =
    Css.Global.selector (String.join "" [ "[data-nri-description=\"", description, "\"]" ])
