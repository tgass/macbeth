#!/usr/bin/env bash
# FIXME This is hack for https://github.com/commercialhaskell/stack/issues/2299
set -e
PFX=$(dirname -- "`stack path --local-pkg-db`")
LIBWXC=$(ls -- $PFX/lib/*-ghc-*/wxc-*/libwxc.so)||true
[[ -n $LIBWXC ]] && env "LD_PRELOAD=$LIBWXC" "$@" || "$@"
