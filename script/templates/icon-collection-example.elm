module Examples.COLLECTION_NAME exposing (example, State, Msg)

{-|

@docs example, State, Msg

-}

import Code
import Example exposing (Example)
import IconExamples as IconExamples exposing (Group)
import Nri.Ui.COLLECTION_NAME.V1 as COLLECTION_NAME


{-| -}
type alias State =
    IconExamples.Settings


{-| -}
type alias Msg =
    IconExamples.Msg


{-| -}
example : Example State Msg
example =
    { moduleName = "COLLECTION_NAME"
    , version = 1
    , label = "FIRST_ICON_MEANING"
    , name = "firstIconName"
    , icon = COLLECTION_NAME.starFilled
    , renderSvgCode = Code.fromModule "COLLECTION_NAME"
    , preview =
        -- TODO: add more icons to the preview
        IconExamples.preview
            [ COLLECTION_NAME.firstIconName
            ]
    , all = all
    }
        |> IconExamples.example


all : List Group
all =
    [ -- TODO: add all icons in nicely-organized named groups
      ( "[Name this sub-collection!]"
      , [ ( "firstIconName", COLLECTION_NAME.firstIconName, [] )
        ]
      )
    ]
