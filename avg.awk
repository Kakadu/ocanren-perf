#!/usr/bin/env sh
# Simple script to evaluate average.
# Moved to separated file because awk # clash with Makefile's $

awk '{a+=$1} END{print a/NR}' $1
