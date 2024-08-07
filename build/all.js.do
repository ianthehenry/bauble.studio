#!/usr/bin/env bash

redo-ifchange mode wasm.js
mode=$(cat mode)

actual_outpath_jfc=$PWD/$3

cd ../studio

redo-ifchange *.ts *.tsx tsconfig.json rollup.config.mjs yarn.lock types/*/*.ts

rollup_args="--no-treeshake --no-indent"
if [[ $mode == "prod" ]]; then
  rollup_args="-p @rollup/plugin-terser"
fi

./node_modules/.bin/rollup -c $rollup_args -o $actual_outpath_jfc
