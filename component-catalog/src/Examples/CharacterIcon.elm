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
    , version = 1
    , label = "Lindy"
    , name = "lindyInstructive"
    , icon = CharacterIcon.lindyInstructive
    , renderSvgCode = \name -> Code.fromModule moduleName name
    , preview = IconExamples.preview [ CharacterIcon.lindyInstructive, CharacterIcon.lindySupportive ]
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
        ]
      )
    ]
