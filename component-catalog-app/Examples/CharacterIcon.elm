module Examples.CharacterIcon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Code
import Css
import Example exposing (Example)
import Examples.IconExamples as IconExamples exposing (Group)
import Nri.Ui.CharacterIcon.V1 as CharacterIcon


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
    , label = "Panda"
    , name = "panda"
    , icon = CharacterIcon.panda
    , renderSvgCode = \name -> Code.fromModule moduleName name
    , preview = IconExamples.preview [ CharacterIcon.panda, CharacterIcon.redPanda ]
    , all = all
    }
        |> IconExamples.example


all : List Group
all =
    [ ( "Scaffolding"
      , [ ( "panda"
          , CharacterIcon.panda
          , [ Css.height (Css.px 50)
            , Css.width (Css.px 50)
            ]
          )
        , ( "redPanda"
          , CharacterIcon.redPanda
          , [ Css.height (Css.px 50)
            , Css.width (Css.px 50)
            ]
          )
        ]
      )
    ]
