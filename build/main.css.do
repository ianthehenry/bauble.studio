#!/usr/bin/env bash

redo-ifchange mode
mode=$(cat mode)

cd ..

redo-ifchange studio/styles/main.css yarn.lock

extra_flags=""
if [[ $mode == "prod" ]]; then
  extra_flags="--use postcss-minify"
fi

node_modules/.bin/postcss studio/styles/main.css \
  --no-map \
  --use autoprefixer \
  $extra_flags
