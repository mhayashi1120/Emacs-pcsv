(require 'ert)
(require 'pcsv)

(defun pcsv-test-get (csv-string)
  (with-temp-buffer
    (insert csv-string)
    (pcsv-parse-buffer)))

(ert-deftest pcsv-normal-test ()
  "Normal csv"
  :tags '(pcsv)
  (should (equal (pcsv-test-get "") '()))
  (should (equal (pcsv-test-get "\n") '((""))))
  (should (equal (pcsv-test-get "a\n") '(("a"))))
  (should (equal (pcsv-test-get "a") '(("a"))))
  (should (equal (pcsv-test-get "a,") '(("a" ""))))
  (should (equal (pcsv-test-get
                  "a,b,c\n\"a,a\",\"bb\n\",\"c,\nc\"\n\"\"\"aaa\"\"\",\",\"\",\"\n") 
                 '(("a" "b" "c") ("a,a" "bb\n" "c,\nc") ("\"aaa\"" ",\",")))))

(ert-deftest pcsv-normal-test ()
  "Invalid csv"
  :tags '(pcsv)
  (should-error (pcsv-test-get "\"a") :type 'invalid-read-syntax))

(ert-deftest pcsv-overflow-test ()
  "Check regexp overflow."
  :tags '(pcsv)
  (should (pcsv-test-get (make-string (ash 1 10) ?a)))
  (should (pcsv-test-get (make-string (ash 1 20) ?a))))
