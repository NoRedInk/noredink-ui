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
    , name = "redSupportive"
    , icon = CharacterIcon.redSupportive
    , renderSvgCode = \name -> Code.fromModule moduleName name
    , preview = IconExamples.preview [ CharacterIcon.lindyInstructive, CharacterIcon.lindySupportive, CharacterIcon.salInstructive, CharacterIcon.salSupportive, CharacterIcon.redInstructive, CharacterIcon.redSupportive ]
    , all = all
    }
        |> IconExamples.example


all : List Group
all =
    [ ( "Scaffolding"
      , [ ( "lindyInstructive"
          , CharacterIcon.lindyInstructive
          , [ Css.width (Css.px 96)
            , Css.height (Css.px 120)
            ]
          )
        , ( "lindySupportive"
          , CharacterIcon.lindySupportive
          , [ Css.width (Css.px 96)
            , Css.height (Css.px 120)
            ]
          )
        , ( "salInstructive"
          , CharacterIcon.salInstructive
          , [ Css.width (Css.px 96)
            , Css.height (Css.px 120)
            ]
          )
        , ( "salSupportive"
          , CharacterIcon.salSupportive
          , [ Css.width (Css.px 96)
            , Css.height (Css.px 120)
            ]
          )
        , ( "redInstructive"
          , CharacterIcon.redInstructive
          , [ Css.width (Css.px 96)
            , Css.height (Css.px 120)
            ]
          )
        , ( "redSupportive"
          , CharacterIcon.redSupportive
          , [ Css.width (Css.px 96)
            , Css.height (Css.px 120)
            ]
          )
        ]
      )
    ]
