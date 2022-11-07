module Nri.Ui.Block.V1 exposing
    ( view, Attribute
    , plaintext, content
    , Content, string, blank
    , emphasize, label
    )

{-|

@docs view, Attribute


## Content

@docs plaintext, content
@docs Content, string, blank


## Customization

@docs emphasize, label

-}

import Accessibility.Styled exposing (..)


{-|

    Block.view [ Block.plaintext "Hello, world!" ]

-}
view : List Attribute -> Html msg
view attributes =
    attributes
        |> List.foldl (\(Attribute attribute) b -> attribute b) defaultConfig
        |> render



-- Attributes


{-| Provide the main content of the block as a plain-text string. You can also use `content` for more complex cases, including a blank appearing within an emphasis.
-}
plaintext : String -> Attribute
plaintext content_ =
    Attribute <| \config -> { config | content = [ String_ content_ ] }


{-| Use `content` for more complex block views, for instance when a blank appears within an emphasis block. Prefer to use `plaintext` when possible for better readability.

    Block.view
        [ Block.emphasize
        , Block.content [ Block.string "Hello, ", Block.blank, Block.string "!" ]
        ]

-}
content : List Content -> Attribute
content content_ =
    Attribute <| \config -> { config | content = content_ }


{-| Mark content as emphasized.
-}
emphasize : Attribute
emphasize =
    Attribute <| \config -> { config | emphasized = True }


{-| -}
label : String -> Attribute
label label_ =
    Attribute <| \config -> { config | label = Just label_ }



-- Content


{-| -}
type Content
    = String_ String
    | Blank


renderContent : Content -> Html msg
renderContent content_ =
    case content_ of
        String_ str ->
            text str

        Blank ->
            text "[Blank -- 1 level down]"


{-| You will only need to use this helper if you're also using `content` to construct a more complex Block. Maybe you want `plaintext` instead?
-}
string : String -> Content
string =
    String_


{-| You will only need to use this helper if you're also using `content` to construct a more complex Block. For a less complex blank Block, don't include content or plaintext in the list of attributes.
-}
blank : Content
blank =
    Blank



-- Internals


{-| -}
type Attribute
    = Attribute (Config -> Config)


defaultConfig : Config
defaultConfig =
    { content = []
    , emphasized = False
    , label = Nothing
    }


type alias Config =
    { content : List Content
    , emphasized : Bool
    , label : Maybe String
    }


render : Config -> Html msg
render config =
    case config.content of
        [] ->
            -- Blank
            text "[Blank]"

        _ ->
            span [] (List.map renderContent config.content)
