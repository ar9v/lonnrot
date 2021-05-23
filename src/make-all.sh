#!/usr/bin/env bash

for f in *.rot
do
    make ${f%.rot}.run
done
