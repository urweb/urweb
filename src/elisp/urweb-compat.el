;;; urweb-compat.el --- Compatibility functions for Emacs variants for urweb-mode

;; Based on sml-mode:
;; Copyright (C) 1999, 2000, 2004  Stefan Monnier <monnier@gnu.org>
;;
;; Modified for urweb-mode:
;; Copyright (C) 2008  Adam Chlipala <adamc@hcoop.net>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Code:

(require 'cl)

(unless (fboundp 'set-keymap-parents)
  (defun set-keymap-parents (m parents)
    (if (keymapp parents) (setq parents (list parents)))
    (set-keymap-parent
     m
     (if (cdr parents)
	 (reduce (lambda (m1 m2)
		   (let ((m (copy-keymap m1)))
		     (set-keymap-parent m m2) m))
		 parents
		 :from-end t)
       (car parents)))))

;; for XEmacs
(when (fboundp 'temp-directory)
  (defvar temporary-file-directory (temp-directory)))

(unless (fboundp 'make-temp-file)
  ;; Copied from Emacs-21's subr.el
  (defun make-temp-file (prefix &optional dir-flag)
  "Create a temporary file.
The returned file name (created by appending some random characters at the end
of PREFIX, and expanding against `temporary-file-directory' if necessary,
is guaranteed to point to a newly created empty file.
You can then use `write-region' to write new data into the file.

If DIR-FLAG is non-nil, create a new empty directory instead of a file."
  (let (file)
    (while (condition-case ()
	       (progn
		 (setq file
		       (make-temp-name
			(expand-file-name prefix temporary-file-directory)))
		 (if dir-flag
		     (make-directory file)
		   (write-region "" nil file nil 'silent))
		 nil)
	    (file-already-exists t))
      ;; the file was somehow created by someone else between
      ;; `make-temp-name' and `write-region', let's try again.
      nil)
    file)))



(unless (fboundp 'regexp-opt)
  (defun regexp-opt (strings &optional paren)
    (let ((open (if paren "\\(" "")) (close (if paren "\\)" "")))
      (concat open (mapconcat 'regexp-quote strings "\\|") close))))


;;;; 
;;;; Custom
;;;; 

;; doesn't exist in Emacs < 20.1
(unless (fboundp 'set-face-bold-p)
  (defun set-face-bold-p (face v &optional f)
    (when v (ignore-errors (make-face-bold face)))))
(unless (fboundp 'set-face-italic-p)
  (defun set-face-italic-p (face v &optional f)
    (when v (ignore-errors (make-face-italic face)))))

;; doesn't exist in Emacs < 20.1
(ignore-errors (require 'custom))
(unless (fboundp 'defgroup)
  (defmacro defgroup (&rest rest) ()))
(unless (fboundp 'defcustom)
  (defmacro defcustom (sym val str &rest rest) `(defvar ,sym ,val ,str)))
(unless (fboundp 'defface)
  (defmacro defface (sym val str &rest rest)
    `(defvar ,sym (make-face ',sym) ,str)))

(defvar :group ':group)
(defvar :type ':type)
(defvar :copy ':copy)
(defvar :dense ':dense)
(defvar :inherit ':inherit)
(defvar :suppress ':suppress)

(provide 'urweb-compat)

;;; urweb-compat.el ends here
