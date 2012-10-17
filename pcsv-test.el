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
  "Check handling huge input."
  :tags '(pcsv)
  (should (pcsv-test-get (make-string (ash 1 10) ?a)))
  (should (pcsv-test-get (make-string (ash 1 20) ?a))))

(ert-deftest pcsv-lazy-parser-test1 ()
  "Check handling lazy parser."
  :tags '(pcsv)
  (with-temp-buffer
    (insert "a,b,c\n")
    (insert "A,B,C\n")
    (let ((parser (pcsv-parser)))
      (should (equal (funcall parser) '("a" "b" "c")))
      (should (equal (funcall parser) '("A" "B" "C")))
      (should-not (funcall parser)))))

(ert-deftest pcsv-lazy-parser-test2 ()
  "Check handling lazy parser."
  :tags '(pcsv)
  (with-temp-buffer
    (insert "a,\"b\",c")
    (let ((parser (pcsv-parser)))
      (should (equal (funcall parser) '("a" "b" "c")))
      (should-not (funcall parser)))))

(ert-deftest pcsv-lazy-file-parser-test ()
  "Check handling lazy file parser."
  :tags '(pcsv)
  (let ((file (make-temp-file "pcsv-")))
    (let ((coding-system-for-write 'euc-jp))
      (write-region "あ,い,う\nA,B,C\n" nil file))
    (let ((parser (pcsv-file-parser file 'euc-jp 1)))
      (should (equal (funcall parser) '("あ" "い" "う")))
      (should (equal (funcall parser) '("A" "B" "C")))
      (should-not (funcall parser)))
    (let ((parser (pcsv-file-parser file 'euc-jp 1)))
      (should (equal (funcall parser) '("あ" "い" "う")))
      (funcall parser t)
      ;; raise no error
      (should-not (funcall parser)))))
