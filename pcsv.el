;;; pcsv.el --- Parser of csv

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: csv parse rfc4180
;; URL: http://github.com/mhayashi1120/Emacs-pcsv/raw/master/pcsv.el
;; URL: http://www.emacswiki.org/emacs/download/pcsv.el
;; Emacs: GNU Emacs 21 or later
;; Version 1.1.1

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; pcsv provides parser of csv based on rfc4180
;; http://www.rfc-editor.org/rfc/rfc4180.txt

;;; Install:

;; Put this file into load-path'ed directory, and byte compile it if
;; desired. And put the following expression into your ~/.emacs.
;;
;;     (require 'pcsv)

;;; Usage:

;; Use `pcsv-parse-buffer', `pcsv-parse-file', `pcsv-parse-region' functions
;; to parse csv.

;;; Code:

(eval-when-compile
  (require 'cl))

(defvar pcsv-separator ?,)

(defvar pcsv-eobp)
(defvar pcsv-quoted-value-regexp)
(defvar pcsv-value-regexp)

(defun pcsv-parse-buffer (&optional buffer)
  "Parse a current buffer as a csv.
BUFFER non-nil means parse buffer instead of current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction
      (widen)
      (pcsv-parse-region (point-min) (point-max)))))

(defun pcsv-parse-file (file &optional coding-system)
  "Parse FILE as a csv file."
  (with-temp-buffer
    (let ((coding-system-for-read coding-system))
      (insert-file-contents file))
    (pcsv-parse-region (point-min) (point-max))))

(defun pcsv-parse-region (start end)
  "Parse region as a csv."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (pcsv-map 'identity))))

(defun pcsv-map (function)
  (save-excursion
    (let ((pcsv-quoted-value-regexp  (pcsv-quoted-value-regexp))
          (pcsv-value-regexp (pcsv-value-regexp))
          pcsv-eobp)
      (goto-char (point-min))
      (loop until (eobp)
            collect (funcall function
                             (loop with v = nil
                                   while (setq v (pcsv-read))
                                   collect v into line
                                   until (bolp)
                                   finally return line))))))

(defun pcsv-quoted-value-regexp ()
  (format "\"\\(\\(?:\"\"\\|[^\"]\\)*\\)\"\\(?:%c\\|\n\\|$\\)" pcsv-separator))

(defun pcsv-value-regexp ()
  (format "\\([^\n%c]*\\)\\(?:%c\\|\n\\|$\\)" pcsv-separator pcsv-separator))

(defun pcsv-read ()
  (cond 
   ((and (not pcsv-eobp)
	 (eobp)
	 (char-before)
	 (= (char-before) pcsv-separator))
    (setq pcsv-eobp t)
    "")
   ((eobp)
    nil)
   ((looking-at "\"")
    (unless (looking-at pcsv-quoted-value-regexp)
      (signal 'invalid-read-syntax nil))
    (prog1 
	(pcsv-unquote-string (match-string 1))
      (goto-char (match-end 0))))
   ((looking-at pcsv-value-regexp)
    (prog1
	(match-string 1)
      (goto-char (match-end 0))))
   (t
    ;; never through here.
    (signal 'invalid-read-syntax nil))))

(defun pcsv-unquote-string (string)
  (loop for i on (string-to-list string)
        by (lambda (x) (if (and (eq (car x) ?\")
                                (eq (cadr x) ?\"))
                           (cddr x)
                         (cdr x)))
        collect (car i) into res
        finally return (concat res)))

(provide 'pcsv)

;;; pcsv.el ends here
