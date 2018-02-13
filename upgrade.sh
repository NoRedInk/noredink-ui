#!/bin/bash
set -Eeuo pipefail

DIRECTORIES="./src/"

perl -p -i -e 's/ Css.File/ DEPRECATED.Css.File/g' `find $DIRECTORIES -name '*.elm'`
perl -p -i -e 's/Css.Namespace/DEPRECATED.Css.Namespace/g' `find $DIRECTORIES -name '*.elm'`
perl -p -i -e 's/import Css( |$)/import Css.Foreign exposing (Snippet, children, descendants, everything, selector)\nimport DEPRECATED.Css.File exposing (Stylesheet, compile, stylesheet)\nimport Css /g' `find $DIRECTORIES -name '*.elm'`
perl -p -i -e 's/Css.text( |$)/Css.text_ /g' `find $DIRECTORIES -name '*.elm'`
perl -p -i -e 's/Css.children/Css.Foreign.children/g' `find $DIRECTORIES -name '*.elm'`
perl -p -i -e 's/Css.selector/Css.Foreign.selector/g' `find $DIRECTORIES -name '*.elm'`
perl -p -i -e 's/Css.class/Css.Foreign.class/g' `find $DIRECTORIES -name '*.elm'`
perl -p -i -e 's/Css.withClass/Css.Foreign.withClass/g' `find $DIRECTORIES -name '*.elm'`
perl -p -i -e 's/Css.descendants/Css.Foreign.descendants/g' `find $DIRECTORIES -name '*.elm'`
perl -p -i -e 's/Css.Elements/Css.Foreign/g' `find $DIRECTORIES -name '*.elm'`
perl -p -i -e 's/Css.Stylesheet/Stylesheet/g' `find $DIRECTORIES -name '*.elm'`
perl -p -i -e 's/Css.stylesheet/stylesheet/g' `find $DIRECTORIES -name '*.elm'`
perl -p -i -e 's/Css.compile/compile/g' `find $DIRECTORIES -name '*.elm'`
perl -p -i -e 's/Css.Snippet/Snippet/g' `find $DIRECTORIES -name '*.elm'`
