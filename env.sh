#!/usr/bin/env bash

# This is hack for https://github.com/commercialhaskell/stack/issues/2299
# Usage: ./env.sh stack build

set -e
PFX=$(dirname -- "`stack path --local-pkg-db`")
LIBWXC=$(ls -- $PFX/lib/*-ghc-*/wxc-*/libwxc.so)||true
[[ -n $LIBWXC ]] && env "LD_PRELOAD=$LIBWXC" "$@" || "$@"
