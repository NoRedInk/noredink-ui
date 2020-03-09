module Nri.Ui.ClickableSvg.V1 exposing
    ( button, link
    , Attribute
    , onClick
    , href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking
    , custom, css
    )

{-|


# Create a button or link

@docs button, link
@docs Attribute


## Behavior

@docs onClick
@docs href, linkSpa, linkExternal, linkWithMethod, linkWithTracking, linkExternalWithTracking


## Sizing

@docs width, height


## Customization

@docs custom, css

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
button : String -> Svg -> List (Attribute msg) -> Html msg
button name icon attributes =
    attributes
        |> List.foldl (\(Attribute attribute) b -> attribute b) (build name icon)
        |> renderButton


{-| -}
link : String -> Svg -> List (Attribute msg) -> Html msg
link name icon attributes =
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



-- SIZING


{-| Default width is 17px.
-}
width : Css.Px -> Attribute msg
width px =
    set (\attributes -> { attributes | width = px })


{-| Default height is 17px.
-}
height : Css.Px -> Attribute msg
height px =
    set (\attributes -> { attributes | height = px })



-- CUSTOMIZATION


{-| Use this helper to add custom attributes.

Do NOT use this helper to add css styles, as they may not be applied the way
you want/expect if underlying Button styles change.
Instead, please use the `css` helper.

-}
custom : List (Html.Attribute msg) -> Attribute msg
custom attributes =
    set
        (\config ->
            { config
                | customAttributes = List.append config.customAttributes attributes
            }
        )


{-| -}
css : List Style -> Attribute msg
css styles =
    set
        (\config ->
            { config
                | customStyles = List.append config.customStyles styles
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
        , height = Css.px 17
        , width = Css.px 17
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
    , height : Css.Px
    , width : Css.Px
    , customAttributes : List (Html.Attribute msg)
    , customStyles : List Style
    }


renderButton : ButtonOrLink msg -> Html msg
renderButton ((ButtonOrLink config) as button_) =
    Html.button
        ([ Attributes.class "Nri-Ui-Clickable-Svg-V1__button"
         , Attributes.type_ "button"
         , Attributes.css (buttonOrLinkStyles config.width config.height ++ config.customStyles)
         , Maybe.map Events.onClick config.onClick
            |> Maybe.withDefault AttributesExtra.none
         , Widget.label config.label
         ]
            ++ config.customAttributes
        )
        [ config.icon
            |> Svg.withWidth config.width
            |> Svg.withHeight config.height
            |> Svg.toHtml
        ]


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
                 , Attributes.css (buttonOrLinkStyles config.width config.height ++ config.customStyles)
                 , Widget.label config.label
                 ]
                    ++ extraAttrs
                    ++ config.customAttributes
                )
                [ config.icon
                    |> Svg.withWidth config.width
                    |> Svg.withHeight config.height
                    |> Svg.toHtml
                ]
    in
    case config.linkType of
        Default ->
            linkBase "link" [ Attributes.target "_self" ]

        SinglePageApp ->
            linkBase "linkSpa"
                (config.onClick
                    |> Maybe.map (\msg -> [ EventExtras.onClickPreventDefaultForLinkWithHref msg ])
                    |> Maybe.withDefault []
                )

        WithMethod method ->
            linkBase "linkWithMethod" [ Attributes.attribute "data-method" method ]

        WithTracking ->
            linkBase
                "linkWithTracking"
                (config.onClick
                    |> Maybe.map (\msg -> [ Events.preventDefaultOn "click" (Json.Decode.succeed ( msg, True )) ])
                    |> Maybe.withDefault []
                )

        External ->
            linkBase "linkExternal" targetBlank

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
                    ]
                )


buttonOrLinkStyles : Css.Px -> Css.Px -> List Style
buttonOrLinkStyles w h =
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
    , Css.width w
    , Css.height h
    ]
