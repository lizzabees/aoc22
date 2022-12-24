#!/usr/bin/env bash
set -e

usage() {
    echo "usage: ghcid.sh <day>"
    echo "  day: aoc day number"
    exit 0
}

if [ $# -lt 1 ]; then usage; fi

ghcid -c "cabal repl day$1" --reload aoc22.cabal 
