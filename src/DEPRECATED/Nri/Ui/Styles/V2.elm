module DEPRECATED.Nri.Ui.Styles.V2 exposing (Keyframe, keyframes, toString)

{-| DEPRECATED. Once we are on elm-css 15.1.0 or later, we should use its
built-in keyframe functionality.


### Keyframe animations

@docs Keyframe, keyframes, toString

-}


{-| A CSS keyframe animation that will have vendor prefixes automatically added.
-}
type Keyframe
    = CompiledKeyframe String


{-| Create a CSS keyframe animation with appropriate vendor prefixes
-}
keyframes : String -> List ( String, String ) -> Keyframe
keyframes name stops =
    let
        stop ( when, what ) =
            when ++ " {" ++ what ++ "}"

        x prefix =
            "@"
                ++ prefix
                ++ "keyframes "
                ++ name
                ++ " {\n"
                ++ String.join "\n" (List.map stop stops)
                ++ "\n}\n"
    in
    [ "-webkit-", "-moz-", "" ]
        |> List.map x
        |> String.join ""
        |> CompiledKeyframe


{-| Turn a [`Keyframe`](#Keyframe) into a string that can be included in a CSS stylesheet.
-}
toString : Keyframe -> String
toString keyframe =
    case keyframe of
        CompiledKeyframe compiled ->
            compiled
