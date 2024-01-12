# pcsv.el

[![CI](https://github.com/mhayashi1120/Emacs-pcsv/actions/workflows/test.yml/badge.svg)](https://github.com/mhayashi1120/Emacs-pcsv/actions/workflows/test.yml)

pcsv provides parser of csv based on rfc4180
http://www.rfc-editor.org/rfc/rfc4180.txt

## Install:

Put this file into load-path'ed directory, and byte compile it if
desired. And put the following expression into your ~/.emacs.

    (require 'pcsv)

## Usage:

Use `pcsv-parse-buffer`, `pcsv-parse-file`, `pcsv-parse-region` functions
to parse csv.

To handle huge csv file, use the lazy parser `pcsv-file-parser`.

To handle csv buffer like cursor, use the `pcsv-parser`.
