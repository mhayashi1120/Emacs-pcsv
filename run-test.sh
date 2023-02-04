#!/bin/sh -e

EMACS="${EMACS:=emacs}"

test -f env.mk && mv -f env.mk env.mk~

echo "ELPA-DIR = ./elpa" >> env.mk
# No extra package pcsv.el
# echo "NEEDED-PACKAGES = " >> env.mk

make ci