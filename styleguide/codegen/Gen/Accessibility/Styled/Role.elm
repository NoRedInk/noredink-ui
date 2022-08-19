module Gen.Accessibility.Styled.Role exposing (alert, alertDialog, application, article, button, checkBox, columnHeader, comboBox, definition, dialog, directory, document, grid, gridCell, group, heading, img, link, list, listBox, listItem, log, marquee, math, menu, menuBar, menuItem, menuItemCheckBox, menuItemRadio, moduleName_, note, option, presentation, progressBar, radio, radioGroup, row, rowGroup, rowHeader, scrollBar, separator, slider, spinButton, status, switch, tab, tabList, tabPanel, textBox, timer, toolBar, toolTip, tree, treeGrid, treeItem, values_)

{-| 
@docs moduleName_, article, comboBox, definition, directory, document, img, link, math, note, alertDialog, dialog, columnHeader, grid, gridCell, row, rowGroup, rowHeader, group, radioGroup, heading, button, checkBox, option, radio, switch, textBox, list, listBox, listItem, alert, log, marquee, timer, status, menu, menuBar, menuItem, menuItemCheckBox, menuItemRadio, progressBar, scrollBar, separator, slider, spinButton, tab, tabList, tabPanel, toolBar, toolTip, tree, treeGrid, treeItem, presentation, application, values_
-}


import Elm
import Elm.Annotation as Type


{-| The name of this module. -}
moduleName_ : List String
moduleName_ =
    [ "Accessibility", "Styled", "Role" ]


{-| Add [`role="article"`](https://www.w3.org/TR/wai-aria-1.1/#article) to the attributes of an element.

article: Html.Styled.Attribute msg
-}
article : Elm.Expression
article =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "article"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="combobox"`](https://www.w3.org/TR/wai-aria-1.1/#combobox) to the attributes of an element.

comboBox: Html.Styled.Attribute msg
-}
comboBox : Elm.Expression
comboBox =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "comboBox"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="definition"`](https://www.w3.org/TR/wai-aria-1.1/#definition) to the attributes of an element.

definition: Html.Styled.Attribute msg
-}
definition : Elm.Expression
definition =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "definition"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="directory"`](https://www.w3.org/TR/wai-aria-1.1/#directory) to the attributes of an element.

directory: Html.Styled.Attribute msg
-}
directory : Elm.Expression
directory =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "directory"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="document"`](https://www.w3.org/TR/wai-aria-1.1/#document) to the attributes of an element.

document: Html.Styled.Attribute msg
-}
document : Elm.Expression
document =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "document"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="img"`](https://www.w3.org/TR/wai-aria-1.1/#img) to the attributes of an element.

img: Html.Styled.Attribute msg
-}
img : Elm.Expression
img =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "img"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="link"`](https://www.w3.org/TR/wai-aria-1.1/#link) to the attributes of an element.

link: Html.Styled.Attribute msg
-}
link : Elm.Expression
link =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "link"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="math"`](https://www.w3.org/TR/wai-aria-1.1/#math) to the attributes of an element.

math: Html.Styled.Attribute msg
-}
math : Elm.Expression
math =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "math"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="note"`](https://www.w3.org/TR/wai-aria-1.1/#note) to the attributes of an element.

note: Html.Styled.Attribute msg
-}
note : Elm.Expression
note =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "note"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="alertdialog"`](https://www.w3.org/TR/wai-aria-1.1/#alertdialog) to the attributes of an element.

alertDialog: Html.Styled.Attribute msg
-}
alertDialog : Elm.Expression
alertDialog =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "alertDialog"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="dialog"`](https://www.w3.org/TR/wai-aria-1.1/#dialog) to the attributes of an element.

dialog: Html.Styled.Attribute msg
-}
dialog : Elm.Expression
dialog =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "dialog"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="columnheader"`](https://www.w3.org/TR/wai-aria-1.1/#columnheader) to the attributes of an element.

columnHeader: Html.Styled.Attribute msg
-}
columnHeader : Elm.Expression
columnHeader =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "columnHeader"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="grid"`](https://www.w3.org/TR/wai-aria-1.1/#grid) to the attributes of an element.

grid: Html.Styled.Attribute msg
-}
grid : Elm.Expression
grid =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "grid"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="gridcell"`](https://www.w3.org/TR/wai-aria-1.1/#gridcell) to the attributes of an element.

gridCell: Html.Styled.Attribute msg
-}
gridCell : Elm.Expression
gridCell =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "gridCell"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="row"`](https://www.w3.org/TR/wai-aria-1.1/#row) to the attributes of an element.

row: Html.Styled.Attribute msg
-}
row : Elm.Expression
row =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "row"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="rowgroup"`](https://www.w3.org/TR/wai-aria-1.1/#rowgroup) to the attributes of an element.

rowGroup: Html.Styled.Attribute msg
-}
rowGroup : Elm.Expression
rowGroup =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "rowGroup"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="rowheader"`](https://www.w3.org/TR/wai-aria-1.1/#rowheader) to the attributes of an element.

rowHeader: Html.Styled.Attribute msg
-}
rowHeader : Elm.Expression
rowHeader =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "rowHeader"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Define a set of controls. (for a group of radio inputs, see radioGroup).

group: Html.Styled.Attribute msg
-}
group : Elm.Expression
group =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "group"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Define a set of radio-controls.

radioGroup: Html.Styled.Attribute msg
-}
radioGroup : Elm.Expression
radioGroup =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "radioGroup"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Prefer using `h1`, `h2`, `h3`, `h4`, `h5`, and `h6`.
Really this attribute should only be necessary if you need an `h7`-type heading.

    div [ heading, level 7 ] []

heading: Html.Styled.Attribute msg
-}
heading : Elm.Expression
heading =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "heading"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="button"`](https://www.w3.org/TR/wai-aria-1.1/#button) to the attributes of an element.

button: Html.Styled.Attribute msg
-}
button : Elm.Expression
button =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "button"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="checkbox"`](https://www.w3.org/TR/wai-aria-1.1/#checkbox) to the attributes of an element.

checkBox: Html.Styled.Attribute msg
-}
checkBox : Elm.Expression
checkBox =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "checkBox"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="option"`](https://www.w3.org/TR/wai-aria-1.1/#option) to the attributes of an element.

option: Html.Styled.Attribute msg
-}
option : Elm.Expression
option =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "option"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="radio"`](https://www.w3.org/TR/wai-aria-1.1/#radio) to the attributes of an element.

radio: Html.Styled.Attribute msg
-}
radio : Elm.Expression
radio =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "radio"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="switch"`](https://www.w3.org/TR/wai-aria-1.1/#switch) to the attributes of an element.

switch: Html.Styled.Attribute msg
-}
switch : Elm.Expression
switch =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "switch"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="textbox"`](https://www.w3.org/TR/wai-aria-1.1/#textbox) to the attributes of an element.

textBox: Html.Styled.Attribute msg
-}
textBox : Elm.Expression
textBox =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "textBox"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="list"`](https://www.w3.org/TR/wai-aria-1.1/#list) to the attributes of an element.

list: Html.Styled.Attribute msg
-}
list : Elm.Expression
list =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "list"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="listbox"`](https://www.w3.org/TR/wai-aria-1.1/#listbox) to the attributes of an element.

listBox: Html.Styled.Attribute msg
-}
listBox : Elm.Expression
listBox =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "listBox"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="listitem"`](https://www.w3.org/TR/wai-aria-1.1/#listitem) to the attributes of an element.

listItem: Html.Styled.Attribute msg
-}
listItem : Elm.Expression
listItem =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "listItem"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="alert"`](https://www.w3.org/TR/wai-aria-1.1/#alert) to the attributes of an element.

alert: Html.Styled.Attribute msg
-}
alert : Elm.Expression
alert =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "alert"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="log"`](https://www.w3.org/TR/wai-aria-1.1/#log) to the attributes of an element.

log: Html.Styled.Attribute msg
-}
log : Elm.Expression
log =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "log"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="marquee"`](https://www.w3.org/TR/wai-aria-1.1/#marquee) to the attributes of an element.

marquee: Html.Styled.Attribute msg
-}
marquee : Elm.Expression
marquee =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "marquee"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="timer"`](https://www.w3.org/TR/wai-aria-1.1/#timer) to the attributes of an element.

timer: Html.Styled.Attribute msg
-}
timer : Elm.Expression
timer =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "timer"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="status"`](https://www.w3.org/TR/wai-aria-1.1/#status) to the attributes of an element.

status: Html.Styled.Attribute msg
-}
status : Elm.Expression
status =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "status"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="menu"`](https://www.w3.org/TR/wai-aria-1.1/#menu) to the attributes of an element.

menu: Html.Styled.Attribute msg
-}
menu : Elm.Expression
menu =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "menu"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="menubar"`](https://www.w3.org/TR/wai-aria-1.1/#menubar) to the attributes of an element.

menuBar: Html.Styled.Attribute msg
-}
menuBar : Elm.Expression
menuBar =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "menuBar"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="menuitem"`](https://www.w3.org/TR/wai-aria-1.1/#menuitem) to the attributes of an element.

menuItem: Html.Styled.Attribute msg
-}
menuItem : Elm.Expression
menuItem =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "menuItem"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="menuitemcheckbox"`](https://www.w3.org/TR/wai-aria-1.1/#menuitemcheckbox) to the attributes of an element.

menuItemCheckBox: Html.Styled.Attribute msg
-}
menuItemCheckBox : Elm.Expression
menuItemCheckBox =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "menuItemCheckBox"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="menuitemradio"`](https://www.w3.org/TR/wai-aria-1.1/#menuitemradio) to the attributes of an element.

menuItemRadio: Html.Styled.Attribute msg
-}
menuItemRadio : Elm.Expression
menuItemRadio =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "menuItemRadio"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="progressbar"`](https://www.w3.org/TR/wai-aria-1.1/#progressbar) to the attributes of an element.

progressBar: Html.Styled.Attribute msg
-}
progressBar : Elm.Expression
progressBar =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "progressBar"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="scrollbar"`](https://www.w3.org/TR/wai-aria-1.1/#scrollbar) to the attributes of an element.

scrollBar: Html.Styled.Attribute msg
-}
scrollBar : Elm.Expression
scrollBar =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "scrollBar"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="separator"`](https://www.w3.org/TR/wai-aria-1.1/#separator) to the attributes of an element.

separator: Html.Styled.Attribute msg
-}
separator : Elm.Expression
separator =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "separator"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="slider"`](https://www.w3.org/TR/wai-aria-1.1/#slider) to the attributes of an element.

slider: Html.Styled.Attribute msg
-}
slider : Elm.Expression
slider =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "slider"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="spinbutton"`](https://www.w3.org/TR/wai-aria-1.1/#spinbutton) to the attributes of an element.

spinButton: Html.Styled.Attribute msg
-}
spinButton : Elm.Expression
spinButton =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "spinButton"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="tab"`](https://www.w3.org/TR/wai-aria-1.1/#tab) to the attributes of an element.

tab: Html.Styled.Attribute msg
-}
tab : Elm.Expression
tab =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "tab"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="tablist"`](https://www.w3.org/TR/wai-aria-1.1/#tablist) to the attributes of an element.

tabList: Html.Styled.Attribute msg
-}
tabList : Elm.Expression
tabList =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "tabList"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="tabpanel"`](https://www.w3.org/TR/wai-aria-1.1/#tabpanel) to the attributes of an element.

tabPanel: Html.Styled.Attribute msg
-}
tabPanel : Elm.Expression
tabPanel =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "tabPanel"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="toolbar"`](https://www.w3.org/TR/wai-aria-1.1/#toolbar) to the attributes of an element.

toolBar: Html.Styled.Attribute msg
-}
toolBar : Elm.Expression
toolBar =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "toolBar"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="tooltip"`](https://www.w3.org/TR/wai-aria-1.1/#tooltip) to the attributes of an element.

toolTip: Html.Styled.Attribute msg
-}
toolTip : Elm.Expression
toolTip =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "toolTip"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="tree"`](https://www.w3.org/TR/wai-aria-1.1/#tree) to the attributes of an element.

tree: Html.Styled.Attribute msg
-}
tree : Elm.Expression
tree =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "tree"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="treegrid"`](https://www.w3.org/TR/wai-aria-1.1/#treegrid) to the attributes of an element.

treeGrid: Html.Styled.Attribute msg
-}
treeGrid : Elm.Expression
treeGrid =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "treeGrid"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Add [`role="treeitem"`](https://www.w3.org/TR/wai-aria-1.1/#treeitem) to the attributes of an element.

treeItem: Html.Styled.Attribute msg
-}
treeItem : Elm.Expression
treeItem =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "treeItem"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Sets role presentation.

presentation: Html.Styled.Attribute msg
-}
presentation : Elm.Expression
presentation =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "presentation"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


{-| Creates a [`role="application"`](https://www.w3.org/TR/wai-aria-1.1/#application) attribute.

**Be very careful with this attribute!** Be sure you fully understand what you're doing before you use it.

application: Html.Styled.Attribute msg
-}
application : Elm.Expression
application =
    Elm.value
        { importFrom = [ "Accessibility", "Styled", "Role" ]
        , name = "application"
        , annotation =
            Just
                (Type.namedWith
                    [ "Html", "Styled" ]
                    "Attribute"
                    [ Type.var "msg" ]
                )
        }


values_ :
    { article : Elm.Expression
    , comboBox : Elm.Expression
    , definition : Elm.Expression
    , directory : Elm.Expression
    , document : Elm.Expression
    , img : Elm.Expression
    , link : Elm.Expression
    , math : Elm.Expression
    , note : Elm.Expression
    , alertDialog : Elm.Expression
    , dialog : Elm.Expression
    , columnHeader : Elm.Expression
    , grid : Elm.Expression
    , gridCell : Elm.Expression
    , row : Elm.Expression
    , rowGroup : Elm.Expression
    , rowHeader : Elm.Expression
    , group : Elm.Expression
    , radioGroup : Elm.Expression
    , heading : Elm.Expression
    , button : Elm.Expression
    , checkBox : Elm.Expression
    , option : Elm.Expression
    , radio : Elm.Expression
    , switch : Elm.Expression
    , textBox : Elm.Expression
    , list : Elm.Expression
    , listBox : Elm.Expression
    , listItem : Elm.Expression
    , alert : Elm.Expression
    , log : Elm.Expression
    , marquee : Elm.Expression
    , timer : Elm.Expression
    , status : Elm.Expression
    , menu : Elm.Expression
    , menuBar : Elm.Expression
    , menuItem : Elm.Expression
    , menuItemCheckBox : Elm.Expression
    , menuItemRadio : Elm.Expression
    , progressBar : Elm.Expression
    , scrollBar : Elm.Expression
    , separator : Elm.Expression
    , slider : Elm.Expression
    , spinButton : Elm.Expression
    , tab : Elm.Expression
    , tabList : Elm.Expression
    , tabPanel : Elm.Expression
    , toolBar : Elm.Expression
    , toolTip : Elm.Expression
    , tree : Elm.Expression
    , treeGrid : Elm.Expression
    , treeItem : Elm.Expression
    , presentation : Elm.Expression
    , application : Elm.Expression
    }
values_ =
    { article =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "article"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , comboBox =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "comboBox"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , definition =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "definition"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , directory =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "directory"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , document =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "document"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , img =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "img"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , link =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "link"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , math =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "math"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , note =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "note"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , alertDialog =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "alertDialog"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , dialog =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "dialog"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , columnHeader =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "columnHeader"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , grid =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "grid"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , gridCell =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "gridCell"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , row =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "row"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , rowGroup =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "rowGroup"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , rowHeader =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "rowHeader"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , group =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "group"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , radioGroup =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "radioGroup"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , heading =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "heading"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , button =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "button"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , checkBox =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "checkBox"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , option =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "option"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , radio =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "radio"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , switch =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "switch"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , textBox =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "textBox"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , list =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "list"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , listBox =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "listBox"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , listItem =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "listItem"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , alert =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "alert"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , log =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "log"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , marquee =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "marquee"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , timer =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "timer"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , status =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "status"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , menu =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "menu"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , menuBar =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "menuBar"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , menuItem =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "menuItem"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , menuItemCheckBox =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "menuItemCheckBox"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , menuItemRadio =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "menuItemRadio"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , progressBar =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "progressBar"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , scrollBar =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "scrollBar"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , separator =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "separator"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , slider =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "slider"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , spinButton =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "spinButton"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , tab =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "tab"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , tabList =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "tabList"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , tabPanel =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "tabPanel"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , toolBar =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "toolBar"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , toolTip =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "toolTip"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , tree =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "tree"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , treeGrid =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "treeGrid"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , treeItem =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "treeItem"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , presentation =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "presentation"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    , application =
        Elm.value
            { importFrom = [ "Accessibility", "Styled", "Role" ]
            , name = "application"
            , annotation =
                Just
                    (Type.namedWith
                        [ "Html", "Styled" ]
                        "Attribute"
                        [ Type.var "msg" ]
                    )
            }
    }


