(require 'el-expectations)
(require 'pcsv)

(defun pcsv-test-get (csv-string)
  (with-temp-buffer
    (insert csv-string)
    (pcsv-parse-buffer)))

(expectations 
  (expect '()
    (pcsv-test-get ""))
  (expect '((""))
    (pcsv-test-get "\n"))
  (expect '(("a"))
    (pcsv-test-get "a\n"))
  (expect '(("a"))
    (pcsv-test-get "a"))
  (expect '(("a" ""))
    (pcsv-test-get "a,"))
  (expect '(("a" "b" "c") ("a,a" "bb\n" "c,\nc") ("\"aaa\"" ",\","))
    (pcsv-test-get "a,b,c\n\"a,a\",\"bb\n\",\"c,\nc\"\n\"\"\"aaa\"\"\",\",\"\",\"\n"))
  (expect (error invalid-read-syntax)
    (pcsv-test-get "\"a"))
  )

(expectations-execute)
