#!/bin/sh
# Run the vliw reference from tools/mlton, where its DATA directory is
cd "$(dirname "$0")" && exec ./vliw.mlton "$@"
