module ClickableAttributes exposing
    ( Config, ClickableAttributes, init
    , onClick, submit, opensModal
    , toButtonAttributes
    , href, linkWithMethod, linkWithTracking
    , linkSpa
    , linkExternal, linkExternalWithTracking
    , toLinkAttributes
    , linkExternalInternal, linkExternalWithTrackingInternal
    )

{-|

@docs Config, ClickableAttributes, init


# For buttons

@docs onClick, submit, opensModal
@docs toButtonAttributes


# For links

@docs href, linkWithMethod, linkWithTracking
@docs linkSpa
@docs linkExternal, linkExternalWithTracking
@docs toLinkAttributes


## external link helpers without any affordances

@docs linkExternalInternal, linkExternalWithTrackingInternal

-}

import Accessibility.Styled.Aria as Aria
import Accessibility.Styled.Role as Role
import EventExtras
import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode
import Nri.Ui.Html.Attributes.V2 as ExtraAttributes exposing (targetBlank)
import Nri.Ui.Svg.V1 as Svg exposing (Svg)
import Nri.Ui.UiIcon.V2 as UiIcon


{-| -}
type alias ClickableAttributes route msg =
    { linkType : Link
    , buttonType : String
    , url : Maybe route
    , urlString : Maybe String
    , onClick : Maybe msg
    , opensModal : Bool
    }


type Link
    = Default
    | WithTracking
    | SinglePageApp
    | WithMethod String
    | External
    | ExternalWithTracking


{-| -}
init : ClickableAttributes route msg
init =
    { linkType = Default
    , buttonType = "button"
    , url = Nothing
    , urlString = Nothing
    , onClick = Nothing
    , opensModal = False
    }


{-| -}
type alias Config attributes route msg =
    { attributes
        | clickableAttributes : ClickableAttributes route msg
        , rightIcon : Maybe Svg
    }


{-| -}
onClick : msg -> Config a route msg -> Config a route msg
onClick msg ({ clickableAttributes } as config) =
    { config | clickableAttributes = { clickableAttributes | onClick = Just msg } }


{-| -}
submit : Config a route msg -> Config a route msg
submit ({ clickableAttributes } as config) =
    { config | clickableAttributes = { clickableAttributes | buttonType = "submit" } }


{-| -}
opensModal : Config a route msg -> Config a route msg
opensModal ({ clickableAttributes } as config) =
    { config | clickableAttributes = { clickableAttributes | opensModal = True } }


{-| -}
href : route -> Config a route msg -> Config a route msg
href url ({ clickableAttributes } as config) =
    { config | clickableAttributes = { clickableAttributes | url = Just url } }


{-| -}
linkSpa : route -> Config a route msg -> Config a route msg
linkSpa url ({ clickableAttributes } as config) =
    { config | clickableAttributes = { clickableAttributes | linkType = SinglePageApp, url = Just url } }


{-| -}
linkWithMethod : { method : String, url : route } -> Config a route msg -> Config a route msg
linkWithMethod { method, url } ({ clickableAttributes } as config) =
    { config | clickableAttributes = { clickableAttributes | linkType = WithMethod method, url = Just url } }


{-| -}
linkWithTracking : { track : msg, url : route } -> Config a route msg -> Config a route msg
linkWithTracking { track, url } ({ clickableAttributes } as config) =
    { config
        | clickableAttributes =
            { clickableAttributes
                | linkType = WithTracking
                , url = Just url
                , onClick = Just track
            }
    }


{-| -}
linkExternal : String -> Config a route msg -> Config a route msg
linkExternal url =
    withExternalAffordance >> linkExternalInternal url


{-| -}
linkExternalWithTracking : { track : msg, url : String } -> Config a route msg -> Config a route msg
linkExternalWithTracking attrs =
    withExternalAffordance >> linkExternalWithTrackingInternal attrs


{-| -}
linkExternalInternal : String -> { attributes | clickableAttributes : ClickableAttributes route msg } -> { attributes | clickableAttributes : ClickableAttributes route msg }
linkExternalInternal url ({ clickableAttributes } as config) =
    { config
        | clickableAttributes =
            { clickableAttributes
                | linkType = External
                , urlString = Just url
            }
    }


{-| -}
linkExternalWithTrackingInternal :
    { track : msg, url : String }
    -> { attributes | clickableAttributes : ClickableAttributes route msg }
    -> { attributes | clickableAttributes : ClickableAttributes route msg }
linkExternalWithTrackingInternal { track, url } ({ clickableAttributes } as config) =
    { config
        | clickableAttributes =
            { clickableAttributes
                | linkType = ExternalWithTracking
                , urlString = Just url
                , onClick = Just track
            }
    }


{-| -}
toButtonAttributes : ClickableAttributes route msg -> { disabled : Bool } -> List (Attribute msg)
toButtonAttributes clickableAttributes { disabled } =
    ExtraAttributes.includeIf clickableAttributes.opensModal
        (Attributes.attribute "aria-haspopup" "true")
        :: (if disabled then
                Aria.disabled True
                    :: (if clickableAttributes.buttonType == "submit" then
                            [ Attributes.type_ "button" ]

                        else
                            [ Attributes.type_ clickableAttributes.buttonType ]
                       )

            else
                [ Attributes.type_ clickableAttributes.buttonType
                , ExtraAttributes.maybe Events.onClick clickableAttributes.onClick
                ]
           )


{-| -}
toLinkAttributes : { routeToString : route -> String, isDisabled : Bool } -> ClickableAttributes route msg -> ( String, List (Attribute msg) )
toLinkAttributes { routeToString, isDisabled } clickableAttributes =
    let
        ( linkTypeName, attributes ) =
            toEnabledLinkAttributes routeToString clickableAttributes
    in
    ( linkTypeName
    , if isDisabled then
        [ Role.link
        , Aria.disabled True
        ]

      else
        attributes
    )


toEnabledLinkAttributes : (route -> String) -> ClickableAttributes route msg -> ( String, List (Attribute msg) )
toEnabledLinkAttributes routeToString clickableAttributes =
    let
        stringUrl =
            case ( clickableAttributes.urlString, clickableAttributes.url ) of
                ( Just url, _ ) ->
                    url

                ( _, Just route ) ->
                    routeToString route

                ( Nothing, Nothing ) ->
                    "#"
    in
    case clickableAttributes.linkType of
        Default ->
            ( "link"
            , [ Attributes.href stringUrl
              , Attributes.target "_self"
              ]
            )

        SinglePageApp ->
            ( "linkSpa"
            , case clickableAttributes.onClick of
                Just handler ->
                    [ Attributes.href stringUrl
                    , EventExtras.onClickPreventDefaultForLinkWithHref handler
                    ]

                Nothing ->
                    [ Attributes.href stringUrl ]
            )

        WithMethod method ->
            ( "linkWithMethod"
            , [ Attributes.href stringUrl
              , Attributes.attribute "data-method" method
              ]
            )

        WithTracking ->
            ( "linkWithTracking"
            , case clickableAttributes.onClick of
                Just track ->
                    [ Attributes.href stringUrl
                    , Events.preventDefaultOn "click"
                        (Json.Decode.succeed ( track, True ))
                    ]

                Nothing ->
                    [ Attributes.href stringUrl ]
            )

        External ->
            ( "linkExternal"
            , Attributes.href stringUrl
                :: targetBlank
            )

        ExternalWithTracking ->
            ( "linkExternalWithTracking"
            , case clickableAttributes.onClick of
                Just handler ->
                    [ Attributes.href stringUrl
                    , Events.onClick handler
                    , Events.on "auxclick" (Json.Decode.succeed handler)
                    ]
                        ++ targetBlank

                Nothing ->
                    Attributes.href stringUrl
                        :: targetBlank
            )


{-| -}
withExternalAffordance : { attributes | rightIcon : Maybe Svg.Svg } -> { attributes | rightIcon : Maybe Svg.Svg }
withExternalAffordance config =
    { config | rightIcon = Just opensInNewTab }


opensInNewTab : Svg
opensInNewTab =
    Svg.withLabel "Opens in a new tab" UiIcon.openInNewTab
