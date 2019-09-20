#!/bin/sh

buildapp --output loam                 \
         --asdf-path .                 \
         --asdf-tree ~/quicklisp/dists \
         --load-system loam            \
         --entry loam:main
