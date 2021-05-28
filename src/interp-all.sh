#!/usr/bin/env bash

for f in *.rot
do
    racket -t interp-file.rkt -m $f
done
