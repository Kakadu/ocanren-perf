#!/usr/bin/env bash
set -x
rm -fr .avg
$@
$@
$@
$@
$@

awk '{a+=$1} END{print a/NR}' .avg
