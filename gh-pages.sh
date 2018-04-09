#!/bin/bash
set -e

rm -rf gh-pages || exit 0;

mkdir -p gh-pages

# copy the assets
cp -r assets/ gh-pages/

# compile JS using Elm
elm make Main.elm --yes --output gh-pages/assets/elm.js

# move the index.html
sed 's/\/_compile\/Main\.elm/elm\.js/g' index.html > gh-pages/index.html

# configure domain
cd gh-pages
echo "fvar.unsoundscapes.com" >> CNAME

# init branch and commit
git init
git add .
git commit -m "Deploying to GH Pages"
git push --force "git@github.com:w0rm/elm-font-dimensions.git" master:gh-pages