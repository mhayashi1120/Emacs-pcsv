# pcsv.el

pcsv provides parser of csv based on rfc4180
http://www.rfc-editor.org/rfc/rfc4180.txt


## Install:

    (require 'pcsv)

## Usage:

Use `pcsv-parse-buffer', `pcsv-parse-file', `pcsv-parse-region' functions
to parse csv.

To handle huge csv file, use the lazy parser `pcsv-file-parser'. (After GNU Emacs 24)

To handle csv buffer like cursor, use the `pcsv-parser'. (After GNU Emacs 24)

