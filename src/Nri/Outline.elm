module Nri.Outline exposing (NodeConfig, NodeLayout, config, html, node, segment, styles)

{-| A module for rendering outline layouts.

@docs segment
@docs node
@docs NodeLayout
@docs NodeConfig
@docs config
@docs html
@docs styles

-}

import Css exposing (Stylesheet)
import Html exposing (Attribute, Html)
import Nri.Colors
import Nri.Effects
import Nri.Palette exposing (Palette)
import Nri.Stylers
import Nri.Styles exposing (Styles)


{-| A wrapper for a node rendered into Html. This type exists to prevent us
from accidentally wrapping a node in a container element before passing it as a
child to another node or segment. Such wrapping would break some of our
styling, which assumes nodes of the same level are sibblings in the Html tree.
-}
type NodeLayout msg
    = NodeLayout (Html msg)


{-| A container to draw nodes in.

    segment
        [ node { config | label = "First Node" }
        , node { config | label = "Second Node" }
        ]

-}
segment : List (NodeLayout msg) -> Html msg
segment children =
    styles.div Segment (List.map unlayout children)


{-| Wrap any html in a NodeLayout so you can use it as sibling content to nodes.

    segment
        [ node { config | label = "This is a node" }
        , html (Html.text "This is some random Html content!")
        ]

-}
html : Html msg -> NodeLayout msg
html child =
    styles.div CustomHtml [ child ]
        |> NodeLayout


{-| Defines how a node should look.
-}
type alias NodeConfig msg =
    -- The node's label.
    { label : Html msg

    -- The content of the node (the part in the colored area).
    , contents : Html msg

    -- Child nodes (and other content) to be placed below the content.
    , children : List (NodeLayout msg)

    -- The node is selected. Draw a selection shadow around it.
    , selected : Bool

    -- The node is ghosted. Fade it out.
    , ghosted : Bool

    -- Addition attributes to be set on the top level node element.
    , attrs : List (Html.Attribute msg)
    }


{-| A default node configuration, allowing you to only set the properties you care about.

    node { config | label = "Claim" }

-}
config : NodeConfig msg
config =
    { label = Html.text ""
    , contents = Html.text ""
    , children = []
    , selected = False
    , ghosted = False
    , attrs = []
    }


{-| Draw a node of an outline structure. You can draw other nodes inside it and
connecting lines will appear.

    node { config | label = "Claim" }

-}
node : NodeConfig msg -> NodeLayout msg
node { label, contents, children, selected, attrs, ghosted } =
    NodeLayout <|
        -- We use a custom Html tag name here, to ensure we can find the first and
        -- last node in a last-of-type selector.
        Html.node "outline-node"
            (styles.classList
                [ ( Node, True )
                , ( GhostedNode, ghosted )
                ]
                :: attrs
            )
            [ Html.div
                [ styles.classList
                    [ ( InnerNode, True )
                    , ( SelectedNode, selected )
                    ]
                ]
                (styles.div Label [ label ]
                    :: styles.div Contents [ contents ]
                    :: List.map unlayout children
                )
            ]


unlayout : NodeLayout msg -> Html msg
unlayout (NodeLayout html) =
    html


type Style
    = Segment
    | Node
    | InnerNode
    | SelectedNode
    | GhostedNode
    | Label
    | Contents
    | CustomHtml


labelHeight : Float
labelHeight =
    35


{-| Styles used by outline structures.
-}
styles : Styles a Style b
styles =
    Nri.Styles.styles "Outline" <|
        [ Css.class Segment
            [ Css.position Css.relative
            , Css.zIndex (Css.int 0)

            -- The overflow property cuts of connecting lines extending from
            -- top level nodes.
            , Css.overflow Css.auto
            ]
        , Css.class Node
            -- The node's relative positioning allows the connecting line to
            -- point upward relative from the node's bounding box.
            [ Css.position Css.relative
            , Css.display Css.block

            -- This selects all nodes on a level but the first.
            , Css.generalSiblings
                [ Css.class Node
                    -- Add some spacing between nodes of the same level.
                    [ Css.marginTop (Css.px 20)
                    , Css.before
                        [ Css.property "content" "''"
                        , Css.borderLeft2 (Css.px 1) Css.solid
                        , Css.batch lineStyles
                        ]
                    ]
                ]

            -- Child nodes have a connecting line and are indented.
            , Css.descendants nestedNodeStyles
            ]
        , Css.class InnerNode
            [ Css.overflow Css.auto

            -- The position and zIndex create a new stacking context. Connecting
            -- lines in child nodes of this one will be drawn in this context.
            , Css.position Css.relative
            , Css.zIndex (Css.int 0)

            -- Recursively assign color styles to the different nested levels of
            -- the outline structure.
            , Css.descendants
                (colorStyles
                    [ Nri.Palette.cornflower
                    , Nri.Palette.aqua
                    , Nri.Palette.turquoise
                    , Nri.Palette.green
                    ]
                )
            ]
        , Css.class GhostedNode
            [ Css.opacity (Css.num 0.5)
            , Css.zIndex (Css.int -1)
            , Css.position Css.relative
            ]
        , Css.class Label
            [ Css.border2 (Css.px 1) Css.solid
            , Css.padding2 Css.zero (Css.px 15)
            , Css.fontSize (Css.px 15)
            , Css.borderRadius (Css.px labelHeight)
            , Css.lineHeight (Css.px (labelHeight - 3))
            , Css.height (Css.px labelHeight)
            , Css.backgroundColor Nri.Colors.white
            , Css.position Css.absolute
            , Css.boxSizing Css.borderBox
            , Css.top Css.zero
            , Css.left Css.zero
            , Nri.Stylers.makeFont (Css.px 15) Nri.Colors.gray20
            , Css.fontWeight Css.bold
            ]
        , Css.class Contents
            [ Css.borderRadius (Css.px 8)
            , Css.marginTop (Css.px (labelHeight / 2))
            , Css.marginLeft (Css.px (labelHeight / 2))
            , Css.minHeight (Css.px 70)

            -- Ensure there's some margin on all sides, so we have the option of
            -- drawing a border around selected contents without it being cut of
            -- by the surrounding inner node.
            , Css.marginRight (Css.px 5)
            , Css.marginBottom (Css.px 5)
            ]
        , Css.class SelectedNode
            [ Css.children
                [ Css.class Contents
                    [ Css.batch Nri.Effects.selectionShadow
                    ]
                ]
            ]
        ]
            ++ curvedConnectingLineStyles


nestedNodeStyles : List Css.Snippet
nestedNodeStyles =
    [ Css.class Node
        [ Css.marginTop (Css.px 20)
        , Css.before
            -- Draw the connect line. It is like an antenna pointing
            -- upward in the direction of the parent node.
            [ Css.property "content" "''"
            , Css.borderLeft2 (Css.px 1) Css.solid
            , Css.borderBottom2 (Css.px 1) Css.solid
            , Css.batch lineStyles
            ]
        , Css.children
            -- Indent this node relative to the parent.
            [ Css.class InnerNode
                [ Css.marginLeft (Css.px 50)
                ]
            ]
        ]
    , Css.class CustomHtml
        [ Css.marginLeft (Css.px 50)
        ]
    ]


{-| The last node is sometimes connected by a curving line to its parent.
Whether this curve should appear or not depends on the level of the node.
-}
curvedConnectingLineStyles : List Css.Snippet
curvedConnectingLineStyles =
    -- 1. Root level nodes are connected with one another using straight lines,
    --    so they are never curved.
    --
    --        Root Node 1
    --         │
    --        Root Node 2
    --
    -- 2. Second level nodes have a curved line when they are the last node of
    --    their level, and if their parent is the last root node. If either
    --    case is not true, they are connected to a straight line that
    --    continues beneath then. This is illustrated in the diagram below. As
    --    you can see only 'Child Node 3' should have a curved connecting line.
    --
    --        Root Node 1
    --         │
    --         ├─ Child Node 1
    --         │
    --        Root Node 2
    --         │
    --         ├─ Child Node 2
    --         │
    --         ╰─ Child Node 3
    --
    [ Css.class Node
        [ Css.lastOfType
            [ Css.descendants
                [ Css.class Node
                    [ Css.lastOfType
                        [ Css.before
                            [ Css.borderRadius (Css.px 8)
                            ]
                        ]
                    ]
                ]
            ]
        ]

    -- 3. Third and lower level nodes always have a curved line when they are
    --    the last node on their level. This is illustrated in the diagram
    --    below. Sub-Child Node 2 has a curved connecting line, even though it's
    --    parent node is not the last node on its level.
    --
    --        Root Node
    --         │
    --         ├─ Child Node 1
    --         │   │
    --         │   ├─ Sub-Child Node 1
    --         │   │
    --         │   ╰─ Sub-Child Node 2
    --         │
    --         ╰─ Child Node 2
    --
    , Css.class Node
        [ Css.descendants
            [ Css.class Node
                [ Css.descendants
                    [ Css.class Node
                        [ Css.lastOfType
                            [ Css.before
                                [ Css.borderRadius (Css.px 8)
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]


colorStyles : List Palette -> List Css.Snippet
colorStyles palettes =
    case palettes of
        [] ->
            []

        palette :: rest ->
            [ Css.class InnerNode
                [ Css.descendants (colorStyles rest)
                ]
            , Css.class Contents
                [ Css.backgroundColor palette.background
                ]
            , Css.class Label
                [ Css.color palette.primary
                , Css.borderColor palette.border
                ]
            ]


lineStyles : List Css.Style
lineStyles =
    [ Css.display Css.block
    , Css.borderColor Nri.Colors.gray75
    , Css.position Css.absolute
    , Css.width (Css.px 30)
    , Css.left (Css.px 30)
    , Css.bottom (Css.calc (Css.pct 100) Css.minus (Css.px (labelHeight / 2)))

    -- Ensure the connecting line is long enough. The containing element will
    -- cut it to size.
    , Css.height (Css.vh 10000)

    -- Make the connecting line go beneath the parent node,
    -- giving the impression it stops when touching the
    -- parent.
    , Css.zIndex (Css.int -100)
    ]
