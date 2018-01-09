module Nri.Outline
    exposing
        ( OutlineLayout
        , container
        , customRow
        , row
        , rowWithExtraContent
        , styles
        , withEvaluated
        , withTitle
        )

{-| A nestedable layout that can be themed.


## Types

@docs OutlineLayout


## Main container

@docs container


## Rows

@docs row, rowWithExtraContent, customRow


## Row modifiers

@docs withTitle, withEvaluated


## Styles

@docs styles

-}

import Html exposing (..)
import Nri.Outline.Styles as Styles exposing (styles)
import Nri.Outline.Types as Types
import Nri.Outline.Utils as Utils
import Nri.Styles


{-| Aliased strictly for exporting
-}
type alias OutlineLayout msg =
    Types.OutlineLayout msg


{-| The row container. This is required as it injects the styles for the rows into a style tag.

    import Html exposing (..)
    import Nri.Outline as Outline

    main : Html msg
    main =
        div []
            [ Outline.container
                [{- Extra attributes go here -}]
                [{- Rows go here -}]
            ]

-}
container : List (Attribute msg) -> List (Types.OutlineLayout msg) -> Html msg
container attributes rows =
    ul (attributes ++ [ styles.class [ Styles.Container ] ])
        (List.map (Utils.rowToHtml Types.Root) rows)


{-| Render an unstyled row with only the outline styles.

    import Html exposing (..)
    import Nri.Outline as Outline

    main : Html msg
    main =
        div []
            [ Outline.container []
                [ Outline.row
                    { content = text "This is my content"
                    , palette = Palette.red
                    , rows = []
                    }
                ]
            ]

-}
row : Types.RowConfig msg -> Types.OutlineLayout msg
row config =
    Types.Row <| Utils.rawRow config


{-| -}
customRow : Types.CustomConfig msg -> Types.OutlineLayout msg
customRow config =
    Types.Row config


{-| Render a row with extra content. This row cannot have child rows.

    import Html exposing (..)
    import Nri.Outline as Outline

    main : Html msg
    main =
        div []
            [ Outline.container []
                [ Outline.rowWithExtraContent
                    { content = text "This is my content"
                    , palette = Palette.red
                    , extraContent = text "My extra content"
                    }
                ]
            ]

-}
rowWithExtraContent : Types.ExtraContenRowConfig msg -> Types.OutlineLayout msg
rowWithExtraContent config =
    Types.Row <|
        Utils.rawRowWithExtraContent config


{-| Render a row with a title

    import Html exposing (..)
    import Nri.Outline as Outline

    main : Html msg
    main =
        div []
            [ Outline.container []
                [ Outline.withTitle "My Title" <|
                    Outline.row
                        { content = text "This is my content"
                        , palette = Palette.red
                        , rows = []
                        }
                ]
            ]

-}
withTitle : String -> Types.OutlineLayout msg -> Types.OutlineLayout msg
withTitle =
    Utils.withTitle


{-| Render a row with 'good' emphasis

    import Html exposing (..)
    import Nri.Outline as Outline
    import Nri.Outline.Types as Types

    main : Html msg
    main =
        div []
            [ Outline.container []
                [ Outline.withEvaluated Types.Good <|
                    Outline.row
                        { content = text "This is my content"
                        , palette = Palette.red
                        , rows = []
                        }
                ]
            ]

-}
withEvaluated : Types.Evaluation -> Types.OutlineLayout msg -> Types.OutlineLayout msg
withEvaluated =
    Utils.withEvaluated


{-| Styles used by this module
-}
styles : Nri.Styles.Styles Never Styles.CssClasses msg
styles =
    Styles.styles
