module Nri.Ui.InputStyles.V4 exposing
    ( label, Theme(..), input
    , inputPaddingVertical, inputLineHeight, textAreaHeight, writingLineHeight, writingPadding, writingPaddingTop, writingMinHeight, defaultMarginTop
    , focusedInputBoxShadow, focusedErrorInputBoxShadow, errorClass, inputClass
    )

{-|


### Changes from V3

    - Remove ContentCreation theme


### Patch changes

  - expose defaultMarginTop


### Changes from V2

  - adds UserGenerated

InputStyles used by the TextInput and TextArea widgets.

@docs label, Theme, input


## Shared hardcoded values

@docs inputPaddingVertical, inputLineHeight, textAreaHeight, writingLineHeight, writingPadding, writingPaddingTop, writingMinHeight, defaultMarginTop
@docs focusedInputBoxShadow, focusedErrorInputBoxShadow, errorClass, inputClass

-}

import Css exposing (..)
import Css.Global
import Nri.Ui.Colors.Extra as ColorsExtra
import Nri.Ui.Colors.V1 exposing (..)
import Nri.Ui.Fonts.V1


{-| -}
type Theme
    = Standard
    | UserGenerated
    | Writing


{-| -}
label : Theme -> Bool -> Style
label theme inError =
    let
        sharedStyles =
            batch
                [ backgroundColor white
                , left (px 10)
                , top zero
                , fontSize (px 12)
                , Nri.Ui.Fonts.V1.baseFont
                , position absolute
                , fontWeight (int 600)
                , borderRadius (px 4)
                , property "transition" "all 0.4s ease"
                ]
    in
    case theme of
        Standard ->
            batch
                [ sharedStyles
                , padding2 (px 2) (px 5)
                , fontSize (px 12)
                , color navy
                , if inError then
                    batch [ color purple ]

                  else
                    batch []
                ]

        UserGenerated ->
            batch
                [ sharedStyles
                , padding2 zero (px 5)
                , fontSize (px 12)
                , color navy
                , if inError then
                    batch [ color purple ]

                  else
                    batch []
                ]

        Writing ->
            batch
                [ sharedStyles
                , padding2 zero (px 5)
                , border3 (px 1) solid gray75
                , borderRadius (px 4)
                , fontSize (px 15)
                , color navy
                , if inError then
                    batch
                        [ color purple
                        , backgroundColor white
                        , borderColor purple
                        ]

                  else
                    batch []
                ]


{-| -}
defaultMarginTop : Float
defaultMarginTop =
    9


{-| -}
focusedInputBoxShadow : String
focusedInputBoxShadow =
    "inset 0 3px 0 0 " ++ ColorsExtra.toCssString glacier


{-| -}
focusedErrorInputBoxShadow : String
focusedErrorInputBoxShadow =
    "inset 0 3px 0 0 " ++ ColorsExtra.toCssString purpleLight


{-| -}
inputClass : String
inputClass =
    "nri-input"


{-| -}
errorClass : String
errorClass =
    "nri-input-error"


{-| In order to use these styles in an input module, you will need to add the class "override-sass-styles". This is because sass styles in the monolith have higher precendence than the class styles here.
-}
input : Theme -> Style
input theme =
    let
        sharedStyles =
            batch
                [ border3 (px 1) solid gray75
                , width (pct 100)
                , borderRadius (px 8)
                , pseudoClass "placeholder"
                    [ color gray45
                    ]
                , color gray20

                -- fix bootstrap
                , display inlineBlock
                , verticalAlign top
                , marginBottom zero
                , marginTop (px defaultMarginTop)
                , boxShadow6 inset zero (px 3) zero zero gray92
                , property "transition" "border-color 0.4s ease"
                , boxSizing borderBox
                , focus
                    [ borderColor azure
                    , Css.outline3 (Css.px 2) Css.solid Css.transparent
                    , property "box-shadow" focusedInputBoxShadow
                    ]
                , Css.Global.withClass errorClass
                    [ borderColor purple
                    , boxShadow6 inset zero (px 3) zero zero purpleLight
                    , focus
                        [ borderColor purple
                        , property "box-shadow" focusedErrorInputBoxShadow
                        ]
                    ]
                ]
    in
    batch
        [ Css.Global.withClass "override-sass-styles"
            [ case theme of
                Standard ->
                    batch
                        [ sharedStyles
                        , padding2 inputPaddingVertical (px 15)
                        , fontSize (px 15)
                        , Nri.Ui.Fonts.V1.baseFont
                        ]

                UserGenerated ->
                    batch
                        [ sharedStyles
                        , padding2 inputPaddingVertical (px 15)
                        , fontSize (px 15)
                        , Nri.Ui.Fonts.V1.ugFont
                        ]

                Writing ->
                    batch
                        [ sharedStyles
                        , Nri.Ui.Fonts.V1.quizFont
                        , fontSize (px 18)
                        , lineHeight writingLineHeight
                        , padding writingPadding
                        , paddingTop writingPaddingTop
                        , focus
                            [ Css.Global.adjacentSiblings
                                [ Css.Global.label
                                    [ backgroundColor azure
                                    , color white
                                    , borderColor azure
                                    , Css.Global.withClass errorClass
                                        [ backgroundColor purple
                                        , color white
                                        , borderColor purple
                                        ]
                                    ]
                                ]
                            ]
                        ]
            ]
        ]


{-| -}
inputPaddingVertical : Px
inputPaddingVertical =
    px 12


{-| -}
inputLineHeight : Px
inputLineHeight =
    px 21


{-| -}
textAreaHeight : Px
textAreaHeight =
    px 100


{-| -}
writingLineHeight : Px
writingLineHeight =
    px 27


{-| -}
writingPadding : Px
writingPadding =
    px 15


{-| -}
writingPaddingTop : Px
writingPaddingTop =
    px 20


{-| -}
writingMinHeight : Px
writingMinHeight =
    px 150
