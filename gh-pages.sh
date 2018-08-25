#!/bin/bash
set -e

rm -rf gh-pages || exit 0;

mkdir -p gh-pages

# copy the assets
cp -r assets gh-pages/

# compile JS using Elm
elm make Main.elm --output gh-pages/assets/elm-temp.js

# minify with uglifyjs
uglifyjs gh-pages/assets/elm-temp.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' | uglifyjs --mangle --output=gh-pages/assets/elm.js

rm gh-pages/assets/elm-temp.js

# move the index.html
cp index.html gh-pages/index.html

# configure domain
cd gh-pages
echo "fvar.unsoundscapes.com" >> CNAME

# init branch and commit
git init
git add .
git commit -m "Deploying to GH Pages"
git push --force "git@github.com:w0rm/elm-font-dimensions.git" master:gh-pages
