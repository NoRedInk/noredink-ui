module Nri.Ui.Html.Attributes.V2 exposing
    ( none, includeIf, maybe
    , targetBlank
    , nriDescription, nriDescriptionSelector
    , testId
    , safeIdWithPrefix, safeId
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
@docs safeIdWithPrefix, safeId

-}

import Css
import Css.Global
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes
import Json.Encode as Encode
import Regex exposing (Regex)


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


{-| Prepends a prefix to the result of safeId.
-}
safeIdWithPrefix : String -> String -> String
safeIdWithPrefix prefix string =
    safeId
        (prefix
            ++ "-"
            ++ string
        )


{-| Creates a lowercased string that is safe to use for HTML IDs.
Removes all groups of unsafe characters and replaces each group with a dash.
prepends "id-" to the result to ensure that the ID starts with a letter. (necessary for CSS selectors including getElementById)
See code pen for examples: <https://codepen.io/ap-nri/pen/OJENQLY>
-}
safeId : String -> String
safeId unsafe =
    let
        nonAlphaNumUnderscoreHyphenAnywhere : Regex
        nonAlphaNumUnderscoreHyphenAnywhere =
            -- any contiguous block of characters that aren't any of
            -- + ASCII letters
            -- + numbers
            -- + underscore
            -- + the hyphen-minus character commonly called "dash" (seen in between "hyphen" and "minus" on this line)
            -- This does not need the + at the end; Regex.replace is global by default
            -- but we pay a penalty for calling the replacement function, so
            -- calling it once per contiguous group is an easy way to cut down on that.
            "[^a-zA-Z0-9_-]+"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never

        alphaAtStart : Regex
        alphaAtStart =
            "^[a-zA-Z]"
                |> Regex.fromString
                |> Maybe.withDefault Regex.never

        prefix : String
        prefix =
            if Regex.contains alphaAtStart unsafe then
                ""

            else
                "id-"

        replaceWithHyphenMinus : any -> String
        replaceWithHyphenMinus _ =
            "-"
    in
    prefix
        ++ Regex.replace
            nonAlphaNumUnderscoreHyphenAnywhere
            replaceWithHyphenMinus
            unsafe
