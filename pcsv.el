;;; pcsv.el --- Parser of csv

;; Author: Masahiro Hayashi <mhayashi1120@gmail.com>
;; Keywords: csv parse rfc4180
;; URL: http://github.com/mhayashi1120/Emacs-pcsv/raw/master/pcsv.el
;; Emacs: GNU Emacs 21 or later
;; Version: 1.2.1

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

(defvar pcsv-separator ?,)

;;;###autoload
(defun pcsv-parse-buffer (&optional buffer)
  "Parse a current buffer as a csv.
BUFFER non-nil means parse buffer instead of current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (pcsv-parse-region (point-min) (point-max))))

;;;###autoload
(defun pcsv-parse-file (file &optional coding-system)
  "Parse FILE as a csv file."
  (with-temp-buffer
    (let ((coding-system-for-read coding-system))
      (insert-file-contents file))
    (pcsv-parse-region (point-min) (point-max))))

;;;###autoload
(defun pcsv-parse-region (start end)
  "Parse region as a csv."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (pcsv-map 'identity))))

(defun pcsv-map (function)
  (save-excursion
    (let (lis)
      (goto-char (point-min))
      (while (not (eobp))
        (setq lis (cons
                   (funcall function (pcsv-read-line))
                   lis)))
      (nreverse lis))))

(defvar pcsv--eobp)
(defvar pcsv--quoting-read)

(defun pcsv-peek ()
  (char-after))

(defun pcsv-read-line ()
  (let (pcsv--eobp v lis)
    (while (and (or (null v) (not (bolp)))
                (setq v (pcsv-read)))
      (setq lis (cons v lis)))
    (nreverse lis)))

(defun pcsv-read-char ()
  (cond
   ((eobp)
    (when pcsv--quoting-read
      ;; must be ended before call this function.
      (signal 'invalid-read-syntax
              (list "Unexpected end of buffer")))
    nil)
   (t
    (forward-char)
    (let* ((c (char-before)))
      (cond
       ((null c))
       ((and pcsv--quoting-read (eq c ?\"))
        (let ((c2 (pcsv-peek)))
          (cond
           ((eq ?\" c2)
            (forward-char)
            (setq c ?\"))
           ((memq c2 `(,pcsv-separator ?\n))
            (forward-char)
            (setq c nil))
           ((null c2)
            ;; end of buffer
            (setq c nil))
           (t
            (signal 'invalid-read-syntax
                    (list (format "Expected `\"' but got `%c'" c2)))))))
       ((null pcsv--quoting-read)
        (cond
         ((eq c pcsv-separator)
          (setq c nil))
         ((eq c ?\n)
          (setq c nil)))))
      c))))

(defun pcsv-read ()
  (let ((c (pcsv-peek))
        lis)
    (cond
     ((null c)
      ;; handling last line has no newline
      (cond
       (pcsv--eobp nil)
       ((= (char-before) pcsv-separator)
        (setq pcsv--eobp t)
        "")))
     (t
      (let ((pcsv--quoting-read
             (when (eq c ?\")
               (forward-char)
               t)))
        (while (setq c (pcsv-read-char))
          (setq lis (cons c lis))))
      (concat (nreverse lis))))))

(provide 'pcsv)

;;; pcsv.el ends here
