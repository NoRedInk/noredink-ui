module ClickableAttributes exposing
    ( ClickableAttributes
    , href
    , init
    , linkExternal
    , linkExternalWithTracking
    , linkSpa
    , linkWithMethod
    , linkWithTracking
    , onClick
    , toButtonAttributes
    , toLinkAttributes
    )

{-| -}

import AttributeExtras exposing (targetBlank)
import EventExtras
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode


{-| -}
type alias ClickableAttributes msg =
    { linkType : Link
    , url : String
    , onClick : Maybe msg
    }


type Link
    = Default
    | WithTracking
    | SinglePageApp
    | WithMethod String
    | External
    | ExternalWithTracking


{-| -}
init : ClickableAttributes msg
init =
    { linkType = Default
    , url = "#"
    , onClick = Nothing
    }


{-| -}
onClick : msg -> ClickableAttributes msg -> ClickableAttributes msg
onClick msg clickableAttributes =
    { clickableAttributes | onClick = Just msg }


{-| -}
href : String -> ClickableAttributes msg -> ClickableAttributes msg
href url clickableAttributes =
    { clickableAttributes | url = url }


{-| -}
linkSpa : String -> ClickableAttributes msg -> ClickableAttributes msg
linkSpa url clickableAttributes =
    { clickableAttributes | linkType = SinglePageApp, url = url }


{-| -}
linkWithMethod : { method : String, url : String } -> ClickableAttributes msg -> ClickableAttributes msg
linkWithMethod { method, url } clickableAttributes =
    { clickableAttributes | linkType = WithMethod method, url = url }


{-| -}
linkWithTracking : { track : msg, url : String } -> ClickableAttributes msg -> ClickableAttributes msg
linkWithTracking { track, url } _ =
    { linkType = WithTracking, url = url, onClick = Just track }


{-| -}
linkExternal : String -> ClickableAttributes msg -> ClickableAttributes msg
linkExternal url clickableAttributes =
    { clickableAttributes | linkType = External, url = url }


{-| -}
linkExternalWithTracking : { track : msg, url : String } -> ClickableAttributes msg -> ClickableAttributes msg
linkExternalWithTracking { track, url } _ =
    { linkType = ExternalWithTracking, url = url, onClick = Just track }


{-| -}
toButtonAttributes : ClickableAttributes msg -> List (Attribute msg)
toButtonAttributes clickableAttributes =
    case clickableAttributes.onClick of
        Just handler ->
            [ Events.onClick handler ]

        Nothing ->
            []


{-| -}
toLinkAttributes : ClickableAttributes msg -> ( String, List (Attribute msg) )
toLinkAttributes clickableAttributes =
    case clickableAttributes.linkType of
        Default ->
            ( "link"
            , [ Attributes.href clickableAttributes.url
              , Attributes.target "_self"
              ]
            )

        SinglePageApp ->
            ( "linkSpa"
            , case clickableAttributes.onClick of
                Just handler ->
                    [ Attributes.href clickableAttributes.url
                    , EventExtras.onClickPreventDefaultForLinkWithHref handler
                    ]

                Nothing ->
                    [ Attributes.href clickableAttributes.url ]
            )

        WithMethod method ->
            ( "linkWithMethod"
            , [ Attributes.href clickableAttributes.url
              , Attributes.attribute "data-method" method
              ]
            )

        WithTracking ->
            ( "linkWithTracking"
            , case clickableAttributes.onClick of
                Just track ->
                    [ Attributes.href clickableAttributes.url
                    , Events.preventDefaultOn "click"
                        (Json.Decode.succeed ( track, True ))
                    ]

                Nothing ->
                    [ Attributes.href clickableAttributes.url ]
            )

        External ->
            ( "linkExternal"
            , Attributes.href clickableAttributes.url
                :: targetBlank
            )

        ExternalWithTracking ->
            ( "linkExternalWithTracking"
            , case clickableAttributes.onClick of
                Just handler ->
                    [ Attributes.href clickableAttributes.url
                    , Events.onClick handler
                    , Events.on "auxclick" (Json.Decode.succeed handler)
                    ]
                        ++ targetBlank

                Nothing ->
                    Attributes.href clickableAttributes.url
                        :: targetBlank
            )
