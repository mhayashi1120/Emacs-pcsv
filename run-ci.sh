#!/bin/sh -e

# This script intendet invoke by GithubAction `CI` job
# Keep as *~ to failed execute by developer.
test -f env.mk && mv -f env.mk env.mk~

echo "ELPA-DIR = ./elpa" >> env.mk
# No extra package pcsv.el
# echo "NEEDED-PACKAGES = " >> env.mk

make ci
