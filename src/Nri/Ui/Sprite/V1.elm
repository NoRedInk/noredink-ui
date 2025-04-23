module Nri.Ui.Sprite.V1 exposing
    ( Sprite, attach
    , SpriteId, use, spriteIdToString
    , bold, italic, underline, list, link, undo, redo
    )

{-|

@docs Sprite, attach
@docs SpriteId, use, spriteIdToString
@docs bold, italic, underline, list, link, undo, redo

-}

import Accessibility.Styled as Html exposing (Html)
import Html.Styled.Attributes
import Svg.Styled as Svg exposing (Svg)
import Svg.Styled.Attributes as Attributes


{-| -}
type Sprite
    = Sprite { id : SpriteId, svg : Svg Never }


toSprite : SpriteId -> Svg Never -> Sprite
toSprite id_ svg =
    Sprite { id = id_, svg = svg }


{-| -}
type SpriteId
    = SpriteId String


{-| -}
spriteIdToString : SpriteId -> String
spriteIdToString (SpriteId id_) =
    id_


{-| Sprites _must_ be attached to the page for them to be usable!

If your icons are missing, please make sure you're using this HTML in your view!

-}
attach : Html Never
attach =
    Html.div [ Html.Styled.Attributes.style "display" "none" ]
        (List.map addSprite allSprites)


addSprite : Sprite -> Svg Never
addSprite (Sprite sprite) =
    Svg.svg []
        [ Svg.symbol [ Attributes.id (spriteIdToString sprite.id) ]
            [ sprite.svg
            ]
        ]


{-| -}
use : SpriteId -> Svg.Svg msg
use spriteId =
    Svg.use [ Attributes.xlinkHref ("#" ++ spriteIdToString spriteId) ] []


{-|

    svg [] [ Sprite.use Sprite.bold ]

-}
bold : SpriteId
bold =
    SpriteId "icon-bold"


{-| -}
italic : SpriteId
italic =
    SpriteId "icon-italic"


{-| -}
underline : SpriteId
underline =
    SpriteId "icon-underline"


{-| -}
list : SpriteId
list =
    SpriteId "icon-list"


{-| -}
link : SpriteId
link =
    SpriteId "icon-link"


{-| -}
undo : SpriteId
undo =
    SpriteId "icon-undo"


{-| -}
redo : SpriteId
redo =
    SpriteId "icon-redo"


allSprites : List Sprite
allSprites =
    [ toSprite bold <|
        Svg.svg
            [ Attributes.width "100%"
            , Attributes.height "100%"
            , Attributes.fill "currentcolor"
            , Attributes.viewBox "0 0 22 22"
            ]
            [ Svg.path
                [ Attributes.d "M6 11H11.8299C13.7082 11 15.2308 9.433 15.2308 7.5C15.2308 5.567 13.7082 4 11.8299 4H8.30769C7.21984 4 6.67591 4 6.33795 4.34171C6 4.68342 6 5.23339 6 6.33333V11ZM6 11V15.6667C6 16.7666 6 17.3166 6.33795 17.6583C6.67591 18 7.21984 18 8.30769 18H12.6667C14.5076 18 16 16.433 16 14.5C16 12.567 14.5076 11 12.6667 11H11.7143"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "3"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.fill "none"
                ]
                []
            ]
    , toSprite italic <|
        Svg.svg
            [ Attributes.width "100%"
            , Attributes.height "100%"
            , Attributes.fill "currentcolor"
            , Attributes.viewBox "0 0 22 22"
            ]
            [ Svg.path
                [ Attributes.d "M11 4H17.4166M7.33331 18.6667L14.6666 4M4.58331 18.6667H11"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.fill "none"
                ]
                []
            ]
    , toSprite underline <|
        Svg.svg
            [ Attributes.width "100%"
            , Attributes.height "100%"
            , Attributes.fill "currentcolor"
            , Attributes.viewBox "0 0 22 22"
            ]
            [ Svg.path
                [ Attributes.d "M5.22222 3V10.5556C5.22222 13.7466 7.80902 16.3333 11 16.3333C14.191 16.3333 16.7778 13.7466 16.7778 10.5556V3M3 19H19"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.fill "none"
                ]
                []
            ]
    , toSprite list <|
        Svg.svg
            [ Attributes.width "100%"
            , Attributes.height "100%"
            , Attributes.fill "currentcolor"
            , Attributes.viewBox "0 0 22 22"
            ]
            [ Svg.path
                [ Attributes.d "M7 4H19"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                ]
                []
            , Svg.path
                [ Attributes.d "M3 4H3.00898"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.fill "none"
                ]
                []
            , Svg.path
                [ Attributes.d "M3 11H3.00898"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.fill "none"
                ]
                []
            , Svg.path
                [ Attributes.d "M3 18H3.00898"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.fill "none"
                ]
                []
            , Svg.path
                [ Attributes.d "M7 11H19"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                ]
                []
            , Svg.path
                [ Attributes.d "M7 18H19"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                ]
                []
            ]
    , toSprite link <|
        Svg.svg
            [ Attributes.width "100%"
            , Attributes.height "100%"
            , Attributes.fill "currentcolor"
            , Attributes.viewBox "0 0 22 22"
            ]
            [ Svg.path
                [ Attributes.d "M9 12.229C9.1416 12.4609 9.3097 12.6804 9.5042 12.8828C10.7117 14.1395 12.5522 14.336 13.9576 13.4722C14.218 13.3121 14.4634 13.1157 14.6872 12.8828L17.9266 9.5114C19.3578 8.02184 19.3578 5.60676 17.9266 4.11718C16.4953 2.6276 14.1748 2.62761 12.7435 4.11718L12.03 4.85978"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.fill "none"
                ]
                []
            , Svg.path
                [ Attributes.d "M9.9703 17.14L9.2565 17.8828C7.82526 19.3724 5.50471 19.3724 4.07345 17.8828C2.64218 16.3932 2.64218 13.9782 4.07345 12.4886L7.31287 9.1172C8.74413 7.62761 11.0647 7.6276 12.4959 9.1172C12.6904 9.3195 12.8584 9.539 13 9.7708"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.fill "none"
                ]
                []
            ]
    , toSprite undo <|
        Svg.svg
            [ Attributes.width "100%"
            , Attributes.height "100%"
            , Attributes.fill "currentcolor"
            , Attributes.viewBox "0 0 22 22"
            ]
            [ Svg.path
                [ Attributes.d "M2 7H14C17.3137 7 20 9.6863 20 13C20 16.3137 17.3137 19 14 19H10"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.fill "none"
                ]
                []
            , Svg.path
                [ Attributes.d "M6 3L4.8462 3.87652C2.94873 5.31801 2 6.03875 2 7C2 7.96125 2.94873 8.68199 4.8462 10.1235L6 11"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.fill "none"
                ]
                []
            ]
    , toSprite redo <|
        Svg.svg
            [ Attributes.width "100%"
            , Attributes.height "100%"
            , Attributes.fill "currentcolor"
            , Attributes.viewBox "0 0 22 22"
            ]
            [ Svg.path
                [ Attributes.d "M20 7H8C4.68629 7 2 9.6863 2 13C2 16.3137 4.68629 19 8 19H12"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.fill "none"
                ]
                []
            , Svg.path
                [ Attributes.d "M16 3L17.1538 3.87652C19.0513 5.31801 20 6.03875 20 7C20 7.96125 19.0513 8.68199 17.1538 10.1235L16 11"
                , Attributes.stroke "currentColor"
                , Attributes.strokeWidth "2"
                , Attributes.strokeLinecap "round"
                , Attributes.strokeLinejoin "round"
                , Attributes.fill "none"
                ]
                []
            ]
    ]
