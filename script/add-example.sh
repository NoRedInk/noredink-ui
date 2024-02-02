#!/usr/bin/env bash
set -euo pipefail

# This interactive script will guide you through adding
# a new component example to the Component Catalog.

echo "Let's add a new component example to the Component Catalog!"
echo ""
read -p "What is the name of the new component? " example_name

echo ""

while true; do
read -p "Is this component a collection of icons? (y/n) " yn
case $yn in
    [yY] )
        echo ""
        echo "Great, we'll use \`IconExamples\` to set up a standard icon collection example."
        echo ""

        template=$( cat script/templates/icon-collection-example.elm )

        read -p "What is the name of one of the icons in the collection? " icon_name
        read -p "What is the meaning of this icon? " icon_meaning

        template="${template//COLLECTION_NAME/"$example_name"}"
        template="${template//FIRST_ICON_MEANING/"$icon_meaning"}"
        template="${template//firstIconName/"$icon_name"}"

        echo "${template}"
        break;;
    [nN] )
        echo ""
        echo "Great, we'll set up a typical blank example page in \`Examples.${example_name}\`."
        echo "🚨 There will be TODOs for you to complete in the file. 🚨"
        echo ""

        template=$( cat script/templates/standard-example.elm )

        template="${template//COMPONENT_NAME/"$example_name"}"

        echo "${template}"
        exit;;
    * ) echo "Please enter Y, y, N, or n to proceed.";;
esac
done

echo "Creating \`Examples.${example_name}\` for you in the component-catalog folder."

printf "${template}" >| component-catalog/src/Examples/"${example_name}.elm"