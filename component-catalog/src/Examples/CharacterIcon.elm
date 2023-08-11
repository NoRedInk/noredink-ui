module Examples.CharacterIcon exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Code
import Css
import Example exposing (Example)
import Examples.IconExamples as IconExamples exposing (Group)
import Nri.Ui.CharacterIcon.V2 as CharacterIcon
import Nri.Ui.Svg.V1


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

    -- We need to start off with a blank example image to avoid axe violations for having duplicated ids
    , icon = Nri.Ui.Svg.V1.init "0 0 480 600" []
    , renderSvgCode = \name -> Code.fromModule moduleName name
    , preview =
        IconExamples.previewCustomSize ( Just 70, Just 80 )
            [ CharacterIcon.redHeadshot
            , CharacterIcon.salHeadshot
            , CharacterIcon.lindyHeadshot
            , CharacterIcon.redInstructive
            ]
    , all = all
    }
        |> IconExamples.example


all : List Group
all =
    [ ( "Headshots"
      , [ ( "lindyHeadshot"
          , CharacterIcon.lindyHeadshot
          , [ Css.width (Css.px 96) ]
          )
        , ( "salHeadshot"
          , CharacterIcon.salHeadshot
          , [ Css.width (Css.px 96) ]
          )
        , ( "redHeadshot"
          , CharacterIcon.redHeadshot
          , [ Css.width (Css.px 96) ]
          )
        ]
      )
    , ( "Flipped Headshots"
      , [ ( "lindyHeadshot"
          , CharacterIcon.lindyHeadshotFlipped
          , [ Css.width (Css.px 96) ]
          )
        , ( "salHeadshot"
          , CharacterIcon.salHeadshotFlipped
          , [ Css.width (Css.px 96) ]
          )
        , ( "redHeadshot"
          , CharacterIcon.redHeadshotFlipped
          , [ Css.width (Css.px 96) ]
          )
        ]
      )
    , ( "Instructive"
      , [ ( "lindyInstructive"
          , CharacterIcon.lindyInstructive
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
        , ( "redInstructive"
          , CharacterIcon.redInstructive
          , [ Css.width (Css.px 96)
            , Css.height (Css.px 120)
            ]
          )
        ]
      )
    , ( "Supportive"
      , [ ( "lindySupportive"
          , CharacterIcon.lindySupportive
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
        , ( "redSupportive"
          , CharacterIcon.redSupportive
          , [ Css.width (Css.px 96)
            , Css.height (Css.px 120)
            ]
          )
        ]
      )
    ]
