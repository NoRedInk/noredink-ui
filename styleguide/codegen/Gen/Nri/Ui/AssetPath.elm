module Gen.Nri.Ui.AssetPath exposing (annotation_, call_, caseOf_, make_, moduleName_, url, values_)

{-| 
@docs moduleName_, url, annotation_, make_, caseOf_, call_, values_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "AssetPath" ]


{-| url: Nri.Ui.AssetPath.Asset -> String -}
url : Elm.Expression -> Elm.Expression
url urlArg =
    Elm.apply
        (Elm.value
            { importFrom = [ "Nri", "Ui", "AssetPath" ]
            , name = "url"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "AssetPath" ] "Asset" []
                        ]
                        Type.string
                    )
            }
        )
        [ urlArg ]


annotation_ : { asset : Type.Annotation }
annotation_ =
    { asset = Type.namedWith [ "Nri", "Ui", "AssetPath" ] "Asset" [] }


make_ : { asset : Elm.Expression -> Elm.Expression }
make_ =
    { asset =
        \ar0 ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "AssetPath" ]
                    , name = "Asset"
                    , annotation = Just (Type.namedWith [] "Asset" [])
                    }
                )
                [ ar0 ]
    }


caseOf_ :
    { asset :
        Elm.Expression
        -> { assetTags_0_0 | asset : Elm.Expression -> Elm.Expression }
        -> Elm.Expression
    }
caseOf_ =
    { asset =
        \assetExpression assetTags ->
            Elm.Case.custom
                assetExpression
                (Type.namedWith [ "Nri", "Ui", "AssetPath" ] "Asset" [])
                [ Elm.Case.branch1
                    "Asset"
                    ( "string.String", Type.string )
                    assetTags.asset
                ]
    }


call_ : { url : Elm.Expression -> Elm.Expression }
call_ =
    { url =
        \urlArg ->
            Elm.apply
                (Elm.value
                    { importFrom = [ "Nri", "Ui", "AssetPath" ]
                    , name = "url"
                    , annotation =
                        Just
                            (Type.function
                                [ Type.namedWith
                                    [ "Nri", "Ui", "AssetPath" ]
                                    "Asset"
                                    []
                                ]
                                Type.string
                            )
                    }
                )
                [ urlArg ]
    }


values_ : { url : Elm.Expression }
values_ =
    { url =
        Elm.value
            { importFrom = [ "Nri", "Ui", "AssetPath" ]
            , name = "url"
            , annotation =
                Just
                    (Type.function
                        [ Type.namedWith [ "Nri", "Ui", "AssetPath" ] "Asset" []
                        ]
                        Type.string
                    )
            }
    }


