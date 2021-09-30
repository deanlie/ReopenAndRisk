#! /bin/sh
# Compare and optionally merge CACHE/working_dir versions of a file named as parameter 1

opendiff ./CACHE/$1 ./$1 