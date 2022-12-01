#!/usr/bin/env bash
set -e

usage() {
    echo "$0 <file>"
    echo "eg: $0 c/day1.c"
    exit 0
}

if [ "$#" -lt 1 ]; then usage; fi
source=$1
shift

out=$(mktemp)
cc -std=c17 -o $out -O2 $source
$out $@
rm $out
