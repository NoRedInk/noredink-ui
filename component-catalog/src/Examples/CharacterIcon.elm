module Examples.CharacterIcon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Code
import Css
import Example exposing (Example)
import Examples.IconExamples as IconExamples exposing (Group)
import Nri.Ui.CharacterIcon.V2 as CharacterIcon


{-| -}
type alias State =
    IconExamples.Settings


{-| -}
type alias Msg =
    IconExamples.Msg


moduleName : String
moduleName =
    "CharacterIcon"


{-| -}
example : Example State Msg
example =
    { moduleName = moduleName
    , version = 2
    , label = "Lindy"
    , name = "redInstructive"
    , icon = CharacterIcon.redInstructive
    , renderSvgCode = \name -> Code.fromModule moduleName name
    , preview = IconExamples.preview [ CharacterIcon.lindyInstructive, CharacterIcon.lindySupportive, CharacterIcon.salInstructive, CharacterIcon.salSupportive, CharacterIcon.redInstructive ]
    , all = all
    }
        |> IconExamples.example


all : List Group
all =
    [ ( "Scaffolding"
      , [ ( "lindyInstructive"
          , CharacterIcon.lindyInstructive
          , [ Css.width (Css.px 48)
            , Css.height (Css.px 64)
            ]
          )
        , ( "lindySupportive"
          , CharacterIcon.lindySupportive
          , [ Css.width (Css.px 48)
            , Css.height (Css.px 64)
            ]
          )
        , ( "salInstructive"
          , CharacterIcon.salInstructive
          , [ Css.width (Css.px 48)
            , Css.height (Css.px 64)
            ]
          )
        , ( "salSupportive"
          , CharacterIcon.salSupportive
          , [ Css.width (Css.px 48)
            , Css.height (Css.px 64)
            ]
          )
        , ( "redInstructive"
          , CharacterIcon.redInstructive
          , [ Css.width (Css.px 48)
            , Css.height (Css.px 64)
            ]
          )
        ]
      )
    ]
