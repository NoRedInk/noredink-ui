module Nri.Ui.ClickableSvg.V1 exposing
    ( button, link
    , Attribute
    , onClick
    , href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking
    )

{-|


# Create a button or link

@docs button, link
@docs Attribute
@docs custom, css


## Behavior

@docs onClick
@docs href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking

-}

import Accessibility.Styled.Widget as Widget
import AttributeExtras exposing (targetBlank)
import Css exposing (Style)
import EventExtras.Styled as EventExtras
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode
import Nri.Ui.Colors.V1 as Colors
import Nri.Ui.Html.Attributes.V2 as AttributesExtra
import Nri.Ui.Svg.V1 as Svg exposing (Svg)


{-| -}
type Attribute msg
    = Attribute (ButtonOrLink msg -> ButtonOrLink msg)


{-| -}
button : String -> List (Attribute msg) -> Svg -> Html msg
button name attributes icon =
    attributes
        |> List.foldl (\(Attribute attribute) b -> attribute b) (build name icon)
        |> renderButton


{-| -}
link : String -> List (Attribute msg) -> Svg -> Html msg
link name attributes icon =
    attributes
        |> List.foldl (\(Attribute attribute) b -> attribute b) (build name icon)
        |> renderLink



-- LINKING, CLICKING, and TRACKING BEHAVIOR


{-| -}
onClick : msg -> Attribute msg
onClick msg =
    set (\attributes -> { attributes | onClick = Just msg })


{-| -}
href : String -> Attribute msg
href url =
    set (\attributes -> { attributes | url = url })


{-| Use this link for routing within a single page app.

This will make a normal <a> tag, but change the Events.onClick behavior to avoid reloading the page.

See <https://github.com/elm-lang/html/issues/110> for details on this implementation.

-}
linkSpa : String -> Attribute msg
linkSpa url =
    set
        (\attributes ->
            { attributes
                | linkType = SinglePageApp
                , url = url
            }
        )


{-| Wrap some text so it looks like a button, but actually is wrapped in an anchor to
some url, and it's an HTTP request (Rails includes JS to make this use the given HTTP method)
-}
linkWithMethod : { method : String, url : String } -> Attribute msg
linkWithMethod { method, url } =
    set
        (\attributes ->
            { attributes
                | linkType = WithMethod method
                , url = url
            }
        )


{-| Wrap some text so it looks like a button, but actually is wrapped in an anchor to some url.
This should only take in messages that result in a Msg that triggers Analytics.trackAndRedirect.
For buttons that trigger other effects on the page, please use Nri.Button.button instead.
-}
linkWithTracking : { track : msg, url : String } -> Attribute msg
linkWithTracking { track, url } =
    set
        (\attributes ->
            { attributes
                | linkType = WithTracking
                , url = url
                , onClick = Just track
            }
        )


{-| Wrap some text so it looks like a button, but actually is wrapped in an anchor to
some url and have it open to an external site
-}
linkExternal : String -> Attribute msg
linkExternal url =
    set
        (\attributes ->
            { attributes
                | linkType = External
                , url = url
            }
        )


{-| Wrap some text so it looks like a button, but actually is wrapped in an anchor to some url and have it open to an external site.
-}
linkExternalWithTracking : { track : msg, url : String } -> Attribute msg
linkExternalWithTracking { track, url } =
    set
        (\attributes ->
            { attributes
                | linkType = ExternalWithTracking
                , url = url
                , onClick = Just track
            }
        )



-- INTERNALS


set :
    (ButtonOrLinkAttributes msg -> ButtonOrLinkAttributes msg)
    -> Attribute msg
set with =
    Attribute (\(ButtonOrLink config) -> ButtonOrLink (with config))


build : String -> Svg -> ButtonOrLink msg
build label icon =
    ButtonOrLink
        { onClick = Nothing
        , url = "#"
        , linkType = Default
        , label = label
        , icon = icon
        , customAttributes = []
        , customStyles = []
        }


type ButtonOrLink msg
    = ButtonOrLink (ButtonOrLinkAttributes msg)


type alias ButtonOrLinkAttributes msg =
    { onClick : Maybe msg
    , url : String
    , linkType : Link
    , label : String
    , icon : Svg
    , customAttributes : List (Html.Attribute msg)
    , customStyles : List Style
    }


renderButton : ButtonOrLink msg -> Html msg
renderButton ((ButtonOrLink config) as button_) =
    Html.button
        ([ Attributes.class "Nri-Ui-Clickable-Svg-V1__button"
         , Attributes.type_ "button"
         , Attributes.css (buttonOrLinkStyles ++ config.customStyles)
         , Maybe.map Events.onClick config.onClick
            |> Maybe.withDefault AttributesExtra.none
         , Widget.label config.label
         ]
            ++ config.customAttributes
        )
        [ Svg.toHtml config.icon ]


type Link
    = Default
    | WithTracking
    | SinglePageApp
    | WithMethod String
    | External
    | ExternalWithTracking


renderLink : ButtonOrLink msg -> Html msg
renderLink ((ButtonOrLink config) as link_) =
    let
        linkBase linkFunctionName extraAttrs =
            Html.a
                ([ Attributes.class ("Nri-Ui-Clickable-Svg-" ++ linkFunctionName)
                 , Attributes.href config.url
                 , Attributes.css (buttonOrLinkStyles ++ config.customStyles)
                 , Widget.label config.label
                 ]
                    ++ extraAttrs
                )
                [ Svg.toHtml config.icon ]
    in
    case config.linkType of
        Default ->
            linkBase "link"
                (Attributes.target "_self" :: config.customAttributes)

        SinglePageApp ->
            linkBase "linkSpa"
                ((Maybe.map EventExtras.onClickPreventDefaultForLinkWithHref config.onClick
                    |> Maybe.withDefault AttributesExtra.none
                 )
                    :: config.customAttributes
                )

        WithMethod method ->
            linkBase "linkWithMethod"
                (Attributes.attribute "data-method" method
                    :: config.customAttributes
                )

        WithTracking ->
            linkBase
                "linkWithTracking"
                ((Maybe.map
                    (\msg ->
                        Events.preventDefaultOn "click"
                            (Json.Decode.succeed ( msg, True ))
                    )
                    config.onClick
                    |> Maybe.withDefault AttributesExtra.none
                 )
                    :: config.customAttributes
                )

        External ->
            linkBase "linkExternal"
                (targetBlank ++ config.customAttributes)

        ExternalWithTracking ->
            linkBase "linkExternalWithTracking"
                (List.concat
                    [ targetBlank
                    , config.onClick
                        |> Maybe.map
                            (\onClickMsg ->
                                [ Events.onClick onClickMsg
                                , Events.on "auxclick" (Json.Decode.succeed onClickMsg)
                                ]
                            )
                        |> Maybe.withDefault []
                    , config.customAttributes
                    ]
                )


buttonOrLinkStyles : List Style
buttonOrLinkStyles =
    [ -- Colors, text decoration, cursor
      Css.cursor Css.pointer
    , Css.backgroundColor Css.transparent
    , Css.color Colors.azure
    , Css.textDecoration Css.none
    , Css.hover [ Css.textDecoration Css.none, Css.color Colors.azure ]
    , Css.visited [ Css.color Colors.azure ]

    -- Margins, borders, padding
    , Css.margin Css.zero
    , Css.padding Css.zero
    , Css.borderWidth Css.zero

    -- Sizing
    , Css.boxSizing Css.borderBox
    , Css.lineHeight (Css.num 1)
    ]
