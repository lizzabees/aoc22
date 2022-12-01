#!/usr/bin/env bash
set -e

usage() {
    echo "$0 <file>"
    echo "eg: $0 cpp/day1.cpp"
    exit 0
}

if [ "$#" -lt 1 ]; then usage; fi
source=$1
shift

out=$(mktemp)
c++ -std=c++20 -stdlib=libc++ -o $out -O2 $source
$out $@
rm $out
