#!/usr/bin/env bash
set -x
rm -fr .avg
/usr/bin/time -f "%U" --append -o .avg $@ 
/usr/bin/time -f "%U" --append -o .avg $@ 
/usr/bin/time -f "%U" --append -o .avg $@ 
/usr/bin/time -f "%U" --append -o .avg $@ 
/usr/bin/time -f "%U" --append -o .avg $@ 

awk '{a+=$1} END{print a/NR}' .avg
