module Gen.Nri.Ui.Palette.V1 exposing (annotation_, aqua, blue, caseOf_, cornflower, darkBlue, darkGray, gray, green, make_, moduleName_, purple, red, turquoise, values_, white)

{-| 
@docs moduleName_, white, gray, darkGray, blue, darkBlue, purple, turquoise, green, red, aqua, cornflower, annotation_, make_, caseOf_, values_
-}


import Elm
import Elm.Annotation as Type
import Elm.Case
import Tuple


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Nri", "Ui", "Palette", "V1" ]


{-| White palette (borders are blue)

white: Nri.Ui.Palette.V1.Palette
-}
white : Elm.Expression
white =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
        , name = "white"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Palette", "V1" ] "Palette" [])
        }


{-| Gray palette

gray: Nri.Ui.Palette.V1.Palette
-}
gray : Elm.Expression
gray =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
        , name = "gray"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Palette", "V1" ] "Palette" [])
        }


{-| Dark Gray palette

darkGray: Nri.Ui.Palette.V1.Palette
-}
darkGray : Elm.Expression
darkGray =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
        , name = "darkGray"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Palette", "V1" ] "Palette" [])
        }


{-| Blue palette

blue: Nri.Ui.Palette.V1.Palette
-}
blue : Elm.Expression
blue =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
        , name = "blue"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Palette", "V1" ] "Palette" [])
        }


{-| Dark blue palette

darkBlue: Nri.Ui.Palette.V1.Palette
-}
darkBlue : Elm.Expression
darkBlue =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
        , name = "darkBlue"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Palette", "V1" ] "Palette" [])
        }


{-| Purple palette

purple: Nri.Ui.Palette.V1.Palette
-}
purple : Elm.Expression
purple =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
        , name = "purple"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Palette", "V1" ] "Palette" [])
        }


{-| Turquoise palette

turquoise: Nri.Ui.Palette.V1.Palette
-}
turquoise : Elm.Expression
turquoise =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
        , name = "turquoise"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Palette", "V1" ] "Palette" [])
        }


{-| Green palette

green: Nri.Ui.Palette.V1.Palette
-}
green : Elm.Expression
green =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
        , name = "green"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Palette", "V1" ] "Palette" [])
        }


{-| Red palette

red: Nri.Ui.Palette.V1.Palette
-}
red : Elm.Expression
red =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
        , name = "red"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Palette", "V1" ] "Palette" [])
        }


{-| Aqua palette

aqua: Nri.Ui.Palette.V1.Palette
-}
aqua : Elm.Expression
aqua =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
        , name = "aqua"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Palette", "V1" ] "Palette" [])
        }


{-| Cornflower palette

cornflower: Nri.Ui.Palette.V1.Palette
-}
cornflower : Elm.Expression
cornflower =
    Elm.value
        { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
        , name = "cornflower"
        , annotation =
            Just (Type.namedWith [ "Nri", "Ui", "Palette", "V1" ] "Palette" [])
        }


annotation_ : { palette : Type.Annotation, paletteName : Type.Annotation }
annotation_ =
    { palette =
        Type.alias
            moduleName_
            "Palette"
            []
            (Type.record
                [ ( "border", Type.namedWith [ "Css" ] "Color" [] )
                , ( "background", Type.namedWith [ "Css" ] "Color" [] )
                , ( "primary", Type.namedWith [ "Css" ] "Color" [] )
                , ( "name"
                  , Type.namedWith
                        [ "Nri", "Ui", "Palette", "V1" ]
                        "PaletteName"
                        []
                  )
                ]
            )
    , paletteName =
        Type.namedWith [ "Nri", "Ui", "Palette", "V1" ] "PaletteName" []
    }


make_ :
    { palette :
        { border : Elm.Expression
        , background : Elm.Expression
        , primary : Elm.Expression
        , name : Elm.Expression
        }
        -> Elm.Expression
    , gray : Elm.Expression
    , darkGray : Elm.Expression
    , blue : Elm.Expression
    , darkBlue : Elm.Expression
    , purple : Elm.Expression
    , turquoise : Elm.Expression
    , red : Elm.Expression
    , green : Elm.Expression
    , white : Elm.Expression
    , cornflower : Elm.Expression
    , aqua : Elm.Expression
    }
make_ =
    { palette =
        \palette_args ->
            Elm.withType
                (Type.alias
                    [ "Nri", "Ui", "Palette", "V1" ]
                    "Palette"
                    []
                    (Type.record
                        [ ( "border", Type.namedWith [ "Css" ] "Color" [] )
                        , ( "background", Type.namedWith [ "Css" ] "Color" [] )
                        , ( "primary", Type.namedWith [ "Css" ] "Color" [] )
                        , ( "name"
                          , Type.namedWith
                                [ "Nri", "Ui", "Palette", "V1" ]
                                "PaletteName"
                                []
                          )
                        ]
                    )
                )
                (Elm.record
                    [ Tuple.pair "border" palette_args.border
                    , Tuple.pair "background" palette_args.background
                    , Tuple.pair "primary" palette_args.primary
                    , Tuple.pair "name" palette_args.name
                    ]
                )
    , gray =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "Gray"
            , annotation = Just (Type.namedWith [] "PaletteName" [])
            }
    , darkGray =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "DarkGray"
            , annotation = Just (Type.namedWith [] "PaletteName" [])
            }
    , blue =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "Blue"
            , annotation = Just (Type.namedWith [] "PaletteName" [])
            }
    , darkBlue =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "DarkBlue"
            , annotation = Just (Type.namedWith [] "PaletteName" [])
            }
    , purple =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "Purple"
            , annotation = Just (Type.namedWith [] "PaletteName" [])
            }
    , turquoise =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "Turquoise"
            , annotation = Just (Type.namedWith [] "PaletteName" [])
            }
    , red =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "Red"
            , annotation = Just (Type.namedWith [] "PaletteName" [])
            }
    , green =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "Green"
            , annotation = Just (Type.namedWith [] "PaletteName" [])
            }
    , white =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "White"
            , annotation = Just (Type.namedWith [] "PaletteName" [])
            }
    , cornflower =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "Cornflower"
            , annotation = Just (Type.namedWith [] "PaletteName" [])
            }
    , aqua =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "Aqua"
            , annotation = Just (Type.namedWith [] "PaletteName" [])
            }
    }


caseOf_ :
    { paletteName :
        Elm.Expression
        -> { paletteNameTags_0_0
            | gray : Elm.Expression
            , darkGray : Elm.Expression
            , blue : Elm.Expression
            , darkBlue : Elm.Expression
            , purple : Elm.Expression
            , turquoise : Elm.Expression
            , red : Elm.Expression
            , green : Elm.Expression
            , white : Elm.Expression
            , cornflower : Elm.Expression
            , aqua : Elm.Expression
        }
        -> Elm.Expression
    }
caseOf_ =
    { paletteName =
        \paletteNameExpression paletteNameTags ->
            Elm.Case.custom
                paletteNameExpression
                (Type.namedWith
                    [ "Nri", "Ui", "Palette", "V1" ]
                    "PaletteName"
                    []
                )
                [ Elm.Case.branch0 "Gray" paletteNameTags.gray
                , Elm.Case.branch0 "DarkGray" paletteNameTags.darkGray
                , Elm.Case.branch0 "Blue" paletteNameTags.blue
                , Elm.Case.branch0 "DarkBlue" paletteNameTags.darkBlue
                , Elm.Case.branch0 "Purple" paletteNameTags.purple
                , Elm.Case.branch0 "Turquoise" paletteNameTags.turquoise
                , Elm.Case.branch0 "Red" paletteNameTags.red
                , Elm.Case.branch0 "Green" paletteNameTags.green
                , Elm.Case.branch0 "White" paletteNameTags.white
                , Elm.Case.branch0 "Cornflower" paletteNameTags.cornflower
                , Elm.Case.branch0 "Aqua" paletteNameTags.aqua
                ]
    }


values_ :
    { white : Elm.Expression
    , gray : Elm.Expression
    , darkGray : Elm.Expression
    , blue : Elm.Expression
    , darkBlue : Elm.Expression
    , purple : Elm.Expression
    , turquoise : Elm.Expression
    , green : Elm.Expression
    , red : Elm.Expression
    , aqua : Elm.Expression
    , cornflower : Elm.Expression
    }
values_ =
    { white =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "white"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Palette", "V1" ]
                        "Palette"
                        []
                    )
            }
    , gray =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "gray"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Palette", "V1" ]
                        "Palette"
                        []
                    )
            }
    , darkGray =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "darkGray"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Palette", "V1" ]
                        "Palette"
                        []
                    )
            }
    , blue =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "blue"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Palette", "V1" ]
                        "Palette"
                        []
                    )
            }
    , darkBlue =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "darkBlue"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Palette", "V1" ]
                        "Palette"
                        []
                    )
            }
    , purple =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "purple"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Palette", "V1" ]
                        "Palette"
                        []
                    )
            }
    , turquoise =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "turquoise"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Palette", "V1" ]
                        "Palette"
                        []
                    )
            }
    , green =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "green"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Palette", "V1" ]
                        "Palette"
                        []
                    )
            }
    , red =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "red"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Palette", "V1" ]
                        "Palette"
                        []
                    )
            }
    , aqua =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "aqua"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Palette", "V1" ]
                        "Palette"
                        []
                    )
            }
    , cornflower =
        Elm.value
            { importFrom = [ "Nri", "Ui", "Palette", "V1" ]
            , name = "cornflower"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Nri", "Ui", "Palette", "V1" ]
                        "Palette"
                        []
                    )
            }
    }


