;;; urweb-mode.el --- Major mode for editing (Standard) ML

;; Based on sml-mode:
;; Copyright (C) 1999,2000,2004  Stefan Monnier
;; Copyright (C) 1994-1997  Matthew J. Morley
;; Copyright (C) 1989       Lars Bo Nielsen
;;
;; Modified for urweb-mode:
;; Copyright (C) 2008  Adam Chlipala <adamc@hcoop.net>

;; Author: Lars Bo Nielsen
;;      Olin Shivers
;;	Fritz Knabe (?)
;;	Steven Gilmore (?)
;;	Matthew Morley <mjm@scs.leeds.ac.uk> (aka <matthew@verisity.com>)
;;	Matthias Blume <blume@cs.princeton.edu> (aka <blume@kurims.kyoto-u.ac.jp>)
;;      (Stefan Monnier) monnier@cs.yale.edu
;;      Adam Chlipala

;; This file is not part of GNU Emacs, but it is distributed under the
;; same conditions.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; HISTORY

;; Still under construction: History obscure, needs a biographer as
;; well as a M-x doctor. Change Log on request.

;; Hacked by Olin Shivers for comint from Lars Bo Nielsen's sml.el.

;; Hacked by Matthew Morley to incorporate Fritz Knabe's hilite and
;; font-lock patterns, some of Steven Gilmore's (reduced) easy-menus,
;; and numerous bugs and bug-fixes.

;;; DESCRIPTION

;; See accompanying info file: urweb-mode.info

;;; FOR YOUR .EMACS FILE

;; If urweb-mode.el lives in some non-standard directory, you must tell
;; emacs where to get it. This may or may not be necessary:

;; (add-to-list 'load-path "~jones/lib/emacs/")

;; Then to access the commands autoload urweb-mode with that command:

;; (load "urweb-mode-startup")

;; urweb-mode-hook is run whenever a new urweb-mode buffer is created.

;;; Code:

(eval-when-compile (require 'cl))
(require 'compile)
(require 'urweb-util)
(require 'urweb-move)
(require 'urweb-defs)
(condition-case nil (require 'skeleton) (error nil))

;;; VARIABLES CONTROLLING INDENTATION

(defcustom urweb-indent-level 4
  "*Indentation of blocks in Ur/Web (see also `urweb-structure-indent')."
  :group 'urweb
  :type '(integer))

(defcustom urweb-indent-args urweb-indent-level
  "*Indentation of args placed on a separate line."
  :group 'urweb
  :type '(integer))

(defcustom urweb-electric-semi-mode nil
  "*If non-nil, `\;' will self insert, reindent the line, and do a newline.
If nil, just insert a `\;'.  (To insert while t, do: \\[quoted-insert] \;)."
  :group 'urweb
  :type 'boolean)

(defcustom urweb-rightalign-and t
  "If non-nil, right-align `and' with its leader.
If nil:					If t:
	datatype a = A				datatype a = A
	and b = B				     and b = B"
  :group 'urweb
  :type 'boolean)

;;; OTHER GENERIC MODE VARIABLES

(defvar urweb-mode-info "urweb-mode"
  "*Where to find Info file for `urweb-mode'.
The default assumes the info file \"urweb-mode.info\" is on Emacs' info
directory path.  If it is not, either put the file on the standard path
or set the variable `urweb-mode-info' to the exact location of this file

  (setq urweb-mode-info \"/usr/me/lib/info/urweb-mode\")

in your .emacs file. You can always set it interactively with the
set-variable command.")

(defvar urweb-mode-hook nil
  "*Run upon entering `urweb-mode'.
This is a good place to put your preferred key bindings.")

;;; CODE FOR Ur/Web-MODE

(defun urweb-mode-info ()
  "Command to access the TeXinfo documentation for `urweb-mode'.
See doc for the variable `urweb-mode-info'."
  (interactive)
  (require 'info)
  (condition-case nil
      (info urweb-mode-info)
    (error (progn
             (describe-variable 'urweb-mode-info)
             (message "Can't find it... set this variable first!")))))


;; font-lock setup

(defconst urweb-keywords-regexp
  (urweb-syms-re "and" "case" "class" "con" "constraint" "constraints"
	       "datatype" "else" "end" "extern" "fn" "map"
	       "fun" "functor" "if" "include"
	       "of" "open" "let" "in"
	       "rec" "sequence" "ensure_index" "sig" "signature" "cookie" "style" "task" "policy"
	       "struct" "structure" "table" "view" "then" "type" "val" "where"
	       "with" "ffi"

               "Name" "Type" "Unit")
  "A regexp that matches any non-SQL keywords of Ur/Web.")

(defconst urweb-sql-keywords-regexp
  (urweb-syms-re "SELECT" "DISTINCT" "FROM" "AS" "WHERE" "SQL" "GROUP" "ORDER" "BY"
                 "HAVING" "LIMIT" "OFFSET" "ALL" "UNION" "INTERSECT" "EXCEPT"
                 "TRUE" "FALSE" "AND" "OR" "NOT" "COUNT" "AVG" "SUM" "MIN" "MAX"
                 "ASC" "DESC" "INSERT" "INTO" "VALUES" "UPDATE" "SET" "DELETE"
                 "PRIMARY" "KEY" "CONSTRAINT" "UNIQUE" "CHECK"
                 "FOREIGN" "REFERENCES" "ON" "NO" "ACTION" "CASCADE" "RESTRICT" "NULL"
                 "JOIN" "INNER" "OUTER" "LEFT" "RIGHT" "FULL" "CROSS" "SELECT1"
                 "IF" "THEN" "ELSE" "COALESCE" "LIKE" "RANDOM")
  "A regexp that matches SQL keywords.")

(defconst urweb-lident-regexp "\\<[a-z_][A-Za-z0-9_']*\\>"
  "A regexp that matches lowercase Ur/Web identifiers.")

(defconst urweb-cident-regexp "\\<[A-Z][A-Za-z0-9_']*\\>"
  "A regexp that matches uppercase Ur/Web identifiers.")

;;; Font-lock settings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The font lock regular expressions.

(defun urweb-in-xml ()
  (save-excursion
    (let (
          (depth 0)
          (finished nil)
          (answer nil)
          (bound (max 0 (- (point) 1024)))
          )
      (while (and (not finished)
                  (re-search-backward "\\(\\([-{}]\\)\\|<\\(/?xml\\)?\\)"
                                      bound t))
        (let ((xml-tag (length (or (match-string 3) "")))
              (ch (match-string 2)))
         (cond
          ((equal ch "{")
           (if (> depth 0)
               (decf depth)
             (setq finished t)))
          ((equal ch "}")
           (incf depth))
          ((= xml-tag 3)
           (if (> depth 0)
               (decf depth)
             (progn
               (setq answer t)
               (setq finished t))))
          ((= xml-tag 4)
           (incf depth))

          ((equal ch "-")
           (if (looking-at "->")
               (setq finished (= depth 0))))

          ((and (= depth 0)
                (not (looking-at "<xml")) ;; ignore <xml/>
                (let ((face (get-text-property (point) 'face)))
                  (funcall (if (listp face) #'member #'equal) 'font-lock-tag-face face)))
           ;; previous code was highlighted as tag, seems we are in xml
           (progn
             (setq answer t)
             (setq finished t)))

          ((= depth 0)
           ;; previous thing was a tag like, but not tag
           ;; seems we are in usual code or comment
           (setq finished t))
          )))
      answer)))

(defun amAttribute (face)
  (if (ignore-errors (save-excursion (backward-word 2) (backward-char 1) (looking-at "<")))
      nil
    face))

(defconst urweb-font-lock-keywords
  `(;;(urweb-font-comments-and-strings)
    ("\\(<\\sw+\\)\\(\\s-\\|\\sw\\|=\\|\"[^\"]*\"\\|{[^}]*}\\)*\\(/?>\\)"
     (1 font-lock-tag-face)
     (3 font-lock-tag-face))
    ("\\(</\\sw+>\\)"
     (1 font-lock-tag-face))
    ("\\([^<>{}]+\\)"
     (1 (if (urweb-in-xml)
            font-lock-string-face
          nil)))

    ("\\<\\(fun\\|and\\)\\s-+\\(\\sw+\\)\\s-+[^ \t\n=]"
     (1 font-lock-keyword-face)
     (2 (amAttribute font-lock-function-name-face)))
    ("\\<\\(\\(data\\)?type\\|con\\|class\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (3 (amAttribute font-lock-type-def-face)))
    ("\\<\\(val\\|table\\|sequence\\|ensure_index\\|cookie\\|style\\|task\\|policy\\)\\s-+\\(\\sw+\\>\\s-*\\)?\\(\\sw+\\)\\s-*[=:]"
     (1 font-lock-keyword-face)
     (3 (amAttribute font-lock-variable-name-face)))
    ("\\<\\(structure\\|functor\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 (amAttribute font-lock-module-def-face)))
    ("\\<\\(signature\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 (amAttribute font-lock-interface-def-face)))

    (,urweb-keywords-regexp . font-lock-keyword-face)
    (,urweb-sql-keywords-regexp . font-lock-sql-face)
    (,urweb-cident-regexp . font-lock-cvariable-face))
  "Regexps matching standard Ur/Web keywords.")

(defface font-lock-type-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight type definitions."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-type-def-face 'font-lock-type-def-face
  "Face name to use for type definitions.")

(defface font-lock-module-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight module definitions."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-module-def-face 'font-lock-module-def-face
  "Face name to use for module definitions.")

(defface font-lock-interface-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight interface definitions."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-interface-def-face 'font-lock-interface-def-face
  "Face name to use for interface definitions.")

(defface font-lock-sql-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight SQL keywords."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-sql-face 'font-lock-sql-face
  "Face name to use for SQL keywords.")

(defface font-lock-cvariable-face
  '((t (:inherit font-lock-type-face)))
  "Font Lock mode face used to highlight capitalized identifiers."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-cvariable-face 'font-lock-cvariable-face
  "Face name to use for capitalized identifiers.")

(defface font-lock-tag-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight XML tags."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-tag-face 'font-lock-tag-face
  "Face name to use for XML tags.")

(defface font-lock-attr-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight XML attributes."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-attr-face 'font-lock-attr-face
  "Face name to use for XML attributes.")

;;
;; Code to handle nested comments and unusual string escape sequences
;;

(defsyntax urweb-syntax-prop-table
  '((?\\ . ".") (?* . "."))
  "Syntax table for text-properties")

;; For Emacsen that have no built-in support for nested comments
(defun urweb-get-depth-st ()
  (save-excursion
    (let* ((disp (if (eq (char-before) ?\)) (progn (backward-char) -1) nil))
	   (_ (backward-char))
	   (disp (if (eq (char-before) ?\() (progn (backward-char) 0) disp))
	   (pt (point)))
      (when disp
	(let* ((depth
		(save-match-data
		  (if (re-search-backward "\\*)\\|(\\*" nil t)
		      (+ (or (get-char-property (point) 'comment-depth) 0)
			 (case (char-after) (?\( 1) (?* 0))
			 disp)
		    0)))
	       (depth (if (> depth 0) depth)))
	  (put-text-property pt (1+ pt) 'comment-depth depth)
	  (when depth urweb-syntax-prop-table))))))

(defconst urweb-font-lock-syntactic-keywords
  `(("^\\s-*\\(\\\\\\)" (1 ',urweb-syntax-prop-table))
    ,@(unless urweb-builtin-nested-comments-flag
	'(("(?\\(\\*\\))?" (1 (urweb-get-depth-st)))))))

(defconst urweb-font-lock-defaults
  '(urweb-font-lock-keywords nil nil ((?_ . "w") (?' . "w")) nil
    (font-lock-syntactic-keywords . urweb-font-lock-syntactic-keywords)))

;;;;
;;;; Imenu support
;;;;

(defvar urweb-imenu-regexp
  (concat "^[ \t]*\\(let[ \t]+\\)?"
	  (regexp-opt (append urweb-module-head-syms
			      '("and" "fun" "datatype" "type")) t)
	  "\\>"))

(defun urweb-imenu-create-index ()
  (let (alist)
    (goto-char (point-max))
    (while (re-search-backward urweb-imenu-regexp nil t)
      (save-excursion
	(let ((kind (match-string 2))
	      (column (progn (goto-char (match-beginning 2)) (current-column)))
	      (location
	       (progn (goto-char (match-end 0))
		      (urweb-forward-spaces)
		      (when (looking-at urweb-tyvarseq-re)
			(goto-char (match-end 0)))
		      (point)))
	      (name (urweb-forward-sym)))
	  ;; Eliminate trivial renamings.
	  (when (or (not (member kind '("structure" "signature")))
		    (progn (search-forward "=")
			   (urweb-forward-spaces)
			   (looking-at "sig\\|struct")))
	    (push (cons (concat (make-string (/ column 2) ?\ ) name) location)
		  alist)))))
    alist))

;;; MORE CODE FOR URWEB-MODE

;;;###autoload (add-to-list 'load-path (file-name-directory load-file-name))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.urs?\\'" . urweb-mode))

;;;###autoload
(defalias 'urweb-mode-derived-from
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode urweb-mode urweb-mode-derived-from "Ur/Web"
  "\\<urweb-mode-map>Major mode for editing Ur/Web code.
This mode runs `urweb-mode-hook' just before exiting.
\\{urweb-mode-map}"
  (set (make-local-variable 'font-lock-defaults) urweb-font-lock-defaults)
  (set (make-local-variable 'font-lock-multiline) 'undecided)
  (set (make-local-variable 'outline-regexp) urweb-outline-regexp)
  (set (make-local-variable 'imenu-create-index-function)
       'urweb-imenu-create-index)
  (set (make-local-variable 'add-log-current-defun-function)
       'urweb-current-fun-name)
  ;; Treat paragraph-separators in comments as paragraph-separators.
  (set (make-local-variable 'paragraph-separate)
       (concat "\\([ \t]*\\*)?\\)?\\(" paragraph-separate "\\)"))
  ;; forward-sexp-function is an experimental variable in my hacked Emacs.
  (set (make-local-variable 'forward-sexp-function) 'urweb-user-forward-sexp)
  ;; For XEmacs
  (easy-menu-add urweb-mode-menu)

  ;; Compatibility.  FIXME: we should use `-' in Emacs-CVS.
  (unless (boundp 'skeleton-positions) (set (make-local-variable '@) nil))

  (local-set-key (kbd "C-c C-c") 'compile)
  (local-set-key (kbd "C-c /") 'urweb-close-matching-tag)

  (urweb-mode-variables))

(defun urweb-mode-variables ()
  (set-syntax-table urweb-mode-syntax-table)
  (setq local-abbrev-table urweb-mode-abbrev-table)
  ;; A paragraph is separated by blank lines or ^L only.

  (set (make-local-variable 'indent-line-function) 'urweb-indent-line)
  (set (make-local-variable 'comment-start) "(* ")
  (set (make-local-variable 'comment-end) " *)")
  (set (make-local-variable 'comment-nested) t)
  ;;(set (make-local-variable 'block-comment-start) "* ")
  ;;(set (make-local-variable 'block-comment-end) "")
  ;; (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-start-skip) "(\\*+\\s-*"))

(defun urweb-funname-of-and ()
  "Name of the function this `and' defines, or nil if not a function.
Point has to be right after the `and' symbol and is not preserved."
  (urweb-forward-spaces)
  (if (looking-at urweb-tyvarseq-re) (goto-char (match-end 0)))
  (let ((sym (urweb-forward-sym)))
    (urweb-forward-spaces)
    (unless (or (member sym '(nil "d="))
		(member (urweb-forward-sym) '("d=")))
      sym)))

;;; INDENTATION !!!

(defun urweb-mark-function ()
  "Synonym for `mark-paragraph' -- sorry.
If anyone has a good algorithm for this..."
  (interactive)
  (mark-paragraph))

(defun urweb-indent-line ()
  "Indent current line of Ur/Web code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
	(indent (max (or (ignore-errors (urweb-calculate-indentation)) 0) 0)))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun urweb-back-to-outer-indent ()
  "Unindents to the next outer level of indentation."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "\t ")
    (let ((start-column (current-column))
          (indent (current-column)))
      (if (> start-column 0)
          (progn
            (save-excursion
              (while (>= indent start-column)
                (if (re-search-backward "^[^\n]" nil t)
                    (setq indent (current-indentation))
                  (setq indent 0))))
            (backward-delete-char-untabify (- start-column indent)))))))

(defun urweb-find-comment-indent ()
  (save-excursion
    (let ((depth 1))
      (while (> depth 0)
	(if (re-search-backward "(\\*\\|\\*)" nil t)
	    (cond
	     ;; FIXME: That's just a stop-gap.
	     ((eq (get-text-property (point) 'face) 'font-lock-string-face))
	     ((looking-at "*)") (incf depth))
	     ((looking-at comment-start-skip) (decf depth)))
	  (setq depth -1)))
      (if (= depth 0)
	  (1+ (current-column))
	nil))))

(defun urweb-empty-line ()
  (save-excursion
    (beginning-of-line)
    (let ((start-pos (point)))
      (end-of-line)
      (not (re-search-backward "[^\n \t]" start-pos t)))))

(defun urweb-seek-back ()
  (while (urweb-empty-line) (previous-line 1)))

(defun urweb-skip-matching-braces ()
  "Skip backwards past matching brace pairs, to calculate XML indentation after quoted Ur code"
  (beginning-of-line)
  (let ((start-pos (point))
        (depth 0))
    (end-of-line)
    (while (re-search-backward "[{}]" start-pos t)
      (cond
       ((looking-at "}")
        (incf depth))
       ((looking-at "{")
        (decf depth))))
    (while (and (> depth 0) (re-search-backward "[{}]" nil t)
      (cond
       ((looking-at "}")
        (incf depth))
       ((looking-at "{")
        (decf depth)))))))

(defun urweb-new-tags ()
  "Decide if the previous line of XML introduced unclosed tags"
  (save-excursion
    (let ((start-pos (point))
          (depth 0)
          (done nil))
      (previous-line 1)
      (urweb-seek-back)
      (urweb-skip-matching-braces)
      (urweb-seek-back)
      (beginning-of-line)
      (while (and (not done) (search-forward "<" start-pos t))
        (cond
         ((or (looking-at " ") (looking-at "="))
          nil)
         ((looking-at "/")
          (if (re-search-forward "[^\\sw]>" start-pos t)
              (when (> depth 0) (decf depth))
            (setq done t)))
         (t
          (if (re-search-forward "[^\\sw]>" start-pos t)
              (if (not (save-excursion (backward-char 2) (looking-at "/")))
                  (incf depth))
            (setq done t)))))
      (and (not done) (> depth 0)))))

(defun urweb-tag-matching-indent ()
  "Seek back to a matching opener tag and get its line's indent"
  (save-excursion
    (end-of-line)
    (search-backward "</" nil t)
    (urweb-tag-matcher)
    (beginning-of-line)
    (current-indentation)))

(defun urweb-close-matching-tag ()
  "Insert a closing XML tag for whatever tag is open at the point."
  (interactive)
  (assert (urweb-in-xml))
  (save-excursion
   (urweb-tag-matcher)
   (re-search-forward "<\\([^ ={/>]+\\)" nil t))
  (let ((tag (match-string-no-properties 1)))
    (insert "</" tag ">")))

(defconst urweb-sql-main-starters
  '("SQL" "SELECT" "INSERT" "UPDATE" "DELETE" "FROM" "SELECT1" "WHERE"))

(defconst urweb-sql-starters
  (append urweb-sql-main-starters
          '("^\\s-+FROM" "WHERE" "GROUP" "ORDER" "HAVING" "LIMIT" "OFFSET"
            "VALUES" "SET")))

(defconst urweb-sql-main-starters-re
  (urweb-syms-re urweb-sql-main-starters))
(defconst urweb-sql-starters-re
  (urweb-syms-re urweb-sql-starters))

(defconst urweb-sql-main-starters-paren-re
  (concat "(" urweb-sql-main-starters-re))

(defun urweb-in-sql ()
  "Check if the point is in a block of SQL syntax."
  (save-excursion
    (let ((start-pos (point))
          (depth 0)
          done
          (good t))
      (when (re-search-backward urweb-sql-main-starters-paren-re nil t)
        (forward-char)
        (while (and (not done) (re-search-forward "[()]" start-pos t))
          (save-excursion
            (backward-char)
            (cond
             ((looking-at ")")
              (cond
               ((= depth 0) (setq done t) (setq good nil))
               (t (decf depth))))
             ((looking-at "(")
              (incf depth)))))
        good))))

(defun urweb-sql-depth ()
  "Check if the point is in a block of SQL syntax.
   Returns the paren nesting depth if so, and nil otherwise."
  (save-excursion
    (let ((depth 0)
          done)
      (while (and (not done)
                  (re-search-backward "[()]" nil t))
        (cond
         ((looking-at ")")
          (decf depth))
         ((looking-at "(")
          (if (looking-at urweb-sql-main-starters-paren-re)
              (setq done t)
            (incf depth)))))
      (max 0 depth))))

(defun urweb-calculate-indentation ()
  (save-excursion
    (beginning-of-line) (skip-chars-forward "\t ")
    (urweb-with-ist
     ;; Indentation for comments alone on a line, matches the
     ;; proper indentation of the next line.
     (when (looking-at "(\\*") (urweb-forward-spaces))
     (let (data
	   (sym (save-excursion (urweb-forward-sym))))
       (or
	;; Allow the user to override the indentation.
	(when (looking-at (concat ".*" (regexp-quote comment-start)
				  "[ \t]*fixindent[ \t]*"
				  (regexp-quote comment-end)))
	  (current-indentation))

	;; Continued comment.
	(and (looking-at "\\*") (urweb-find-comment-indent))

        (and (urweb-in-xml)
             (let ((prev-indent (save-excursion
                                  (previous-line 1)
                                  (urweb-seek-back)
                                  (urweb-skip-matching-braces)
                                  (urweb-seek-back)
                                  (current-indentation))))
               (cond
                ((looking-at "</")
                 (urweb-tag-matching-indent))
                ((urweb-new-tags)
                 (+ prev-indent 2))
                (t
                 prev-indent))))

	;; Continued string ? (Added 890113 lbn)
	(and (looking-at "\\\\")
	     (save-excursion
	       (if (save-excursion (previous-line 1)
				   (beginning-of-line)
				   (looking-at "[\t ]*\\\\"))
		   (progn (previous-line 1) (current-indentation))
		 (if (re-search-backward "[^\\\\]\"" nil t)
		     (1+ (current-column))
		   0))))

	;; Closing parens.  Could be handled below with `urweb-indent-relative'?
	(and (looking-at "\\s)")
	     (save-excursion
	       (skip-syntax-forward ")")
	       (backward-sexp 1)
	       (if (urweb-dangling-sym)
		   (urweb-indent-default 'noindent)
		 (current-column))))

        (and (or (looking-at "FROM") (looking-at urweb-sql-starters-re))

             (save-excursion
               (and (re-search-backward urweb-sql-starters-re nil t)
                    (if (looking-at urweb-sql-main-starters-re)
                        (current-column)
                      (current-indentation)))))

        (and (urweb-in-sql)
             (setq data (urweb-sql-depth))
             (save-excursion
               (re-search-backward urweb-sql-starters-re nil t)
               (+ (current-column) 2 (* 2 data))))

	(and (setq data (assoc sym urweb-close-paren))
	     (urweb-indent-relative sym data))

	(and (member sym urweb-starters-syms)
	     (urweb-indent-starter sym))

	(and (string= sym "|") (urweb-indent-pipe))

	(urweb-indent-arg)
	(urweb-indent-default))))))

(defsubst urweb-bolp ()
  (save-excursion (skip-chars-backward " \t|") (bolp)))

(defun urweb-indent-starter (orig-sym)
  "Return the indentation to use for a symbol in `urweb-starters-syms'.
Point should be just before the symbol ORIG-SYM and is not preserved."
  (let ((sym (unless (save-excursion (urweb-backward-arg))
	       (urweb-backward-spaces)
	       (urweb-backward-sym))))
    (if (member sym '(";" "d=")) (setq sym nil))
    (if sym (urweb-get-sym-indent sym)
      ;; FIXME: this can take a *long* time !!
      (setq sym (urweb-find-matching-starter urweb-starters-syms))
      ;; Don't align with `and' because it might be specially indented.
      (if (and (or (equal orig-sym "and") (not (equal sym "and")))
	       (urweb-bolp))
	  (+ (current-column)
	     (if (and urweb-rightalign-and (equal orig-sym "and"))
		 (- (length sym) 3) 0))
	(urweb-indent-starter orig-sym)))))

(defun urweb-indent-relative (sym data)
  (save-excursion
    (urweb-forward-sym) (urweb-backward-sexp nil)
    (unless (second data) (urweb-backward-spaces) (urweb-backward-sym))
    (+ (or (cdr (assoc sym urweb-symbol-indent)) 0)
       (urweb-delegated-indent))))

(defun urweb-indent-pipe ()
  (let ((sym (urweb-find-matching-starter urweb-pipeheads
					(urweb-op-prec "|" 'back))))
    (when sym
      (if (string= sym "|")
	  (if (urweb-bolp) (current-column) (urweb-indent-pipe))
	(let ((pipe-indent (or (cdr (assoc "|" urweb-symbol-indent)) -2)))
	  (when (or (member sym '("datatype"))
		    (and (equal sym "and")
			 (save-excursion
			   (forward-word 1)
			   (not (urweb-funname-of-and)))))
	    (re-search-forward "="))
	  (urweb-forward-sym)
	  (urweb-forward-spaces)
	  (+ pipe-indent (current-column)))))))

(defun urweb-find-forward (re)
  (urweb-forward-spaces)
  (while (and (not (looking-at re))
	      (progn
		(or (ignore-errors (forward-sexp 1) t) (forward-char 1))
		(urweb-forward-spaces)
		(not (looking-at re))))))

(defun urweb-indent-arg ()
  (and (save-excursion (ignore-errors (urweb-forward-arg)))
       ;;(not (looking-at urweb-not-arg-re))
       ;; looks like a function or an argument
       (urweb-move-if (urweb-backward-arg))
       ;; an argument
       (if (save-excursion (not (urweb-backward-arg)))
	   ;; a first argument
	   (+ (current-column) urweb-indent-args)
	 ;; not a first arg
	 (while (and (/= (current-column) (current-indentation))
		     (urweb-move-if (urweb-backward-arg))))
	 (unless (save-excursion (urweb-backward-arg))
	   ;; all earlier args are on the same line
	   (urweb-forward-arg) (urweb-forward-spaces))
	 (current-column))))

(defun urweb-get-indent (data sym)
  (let (d)
    (cond
     ((not (listp data)) data)
     ((setq d (member sym data)) (cadr d))
     ((and (consp data) (not (stringp (car data)))) (car data))
     (t urweb-indent-level))))

(defun urweb-dangling-sym ()
  "Non-nil if the symbol after point is dangling.
The symbol can be an Ur/Web symbol or an open-paren. \"Dangling\" means that
it is not on its own line but is the last element on that line."
  (save-excursion
    (and (not (urweb-bolp))
	 (< (urweb-point-after (end-of-line))
	    (urweb-point-after (or (urweb-forward-sym) (skip-syntax-forward "("))
			     (urweb-forward-spaces))))))

(defun urweb-delegated-indent ()
  (if (urweb-dangling-sym)
      (urweb-indent-default 'noindent)
    (urweb-move-if (backward-word 1)
		 (looking-at urweb-agglomerate-re))
    (current-column)))

(defun urweb-get-sym-indent (sym &optional style)
  "Find the indentation for the SYM we're `looking-at'.
If indentation is delegated, point will move to the start of the parent.
Optional argument STYLE is currently ignored."
;;(assert (equal sym (save-excursion (urweb-forward-sym))))
  (save-excursion
    (let ((delegate (and (not (equal sym "end")) (assoc sym urweb-close-paren)))
	  (head-sym sym))
      (when (and delegate (not (eval (third delegate))))
	;;(urweb-find-match-backward sym delegate)
	(urweb-forward-sym) (urweb-backward-sexp nil)
	(setq head-sym
	      (if (second delegate)
		  (save-excursion (urweb-forward-sym))
		(urweb-backward-spaces) (urweb-backward-sym))))
      (let ((idata (assoc head-sym urweb-indent-rule)))
	(when idata
	  ;;(if (or style (not delegate))
	  ;; normal indentation
	  (let ((indent (urweb-get-indent (cdr idata) sym)))
	    (when indent (+ (urweb-delegated-indent) indent)))
	  ;; delgate indentation to the parent
	  ;;(urweb-forward-sym) (urweb-backward-sexp nil)
	  ;;(let* ((parent-sym (save-excursion (urweb-forward-sym)))
	  ;;     (parent-indent (cdr (assoc parent-sym urweb-indent-starters))))
	  ;; check the special rules
	  ;;(+ (urweb-delegated-indent)
	  ;; (or (urweb-get-indent (cdr indent-data) 1 'strict)
	  ;; (urweb-get-indent (cdr parent-indent) 1 'strict)
	  ;; (urweb-get-indent (cdr indent-data) 0)
	  ;; (urweb-get-indent (cdr parent-indent) 0))))))))
	  )))))

(defun urweb-indent-default (&optional noindent)
  (condition-case nil
      (progn
        (let* ((sym-after (save-excursion (urweb-forward-sym)))
               (_ (urweb-backward-spaces))
               (sym-before (urweb-backward-sym))
               (sym-indent (and sym-before (urweb-get-sym-indent sym-before)))
               (indent-after (or (cdr (assoc sym-after urweb-symbol-indent)) 0)))
          (when (equal sym-before "end")
            ;; I don't understand what's really happening here, but when
            ;; it's `end' clearly, we need to do something special.
            (forward-word 1)
            (setq sym-before nil sym-indent nil))
          (cond
           (sym-indent
            ;; the previous sym is an indentation introducer: follow the rule
            (if noindent
                ;;(current-column)
                sym-indent
              (+ sym-indent indent-after)))
           ;; If we're just after a hanging open paren.
           ((and (eq (char-syntax (preceding-char)) ?\()
                 (save-excursion (backward-char) (urweb-dangling-sym)))
            (backward-char)
            (urweb-indent-default))
           (t
            ;; default-default
            (let* ((prec-after (urweb-op-prec sym-after 'back))
                   (prec (or (urweb-op-prec sym-before 'back) prec-after 100)))
              ;; go back until you hit a symbol that has a lower prec than the
              ;; "current one", or until you backed over a sym that has the same prec
              ;; but is at the beginning of a line.
              (while (and (not (urweb-bolp))
                          (while (urweb-move-if (urweb-backward-sexp (1- prec))))
                          (not (urweb-bolp)))
                (while (urweb-move-if (urweb-backward-sexp prec))))
              (if noindent
                  ;; the `noindent' case does back over an introductory symbol
                  ;; such as `fun', ...
                  (progn
                    (urweb-move-if
                     (urweb-backward-spaces)
                     (member (urweb-backward-sym) urweb-starters-syms))
                    (current-column))
                ;; Use `indent-after' for cases such as when , or ; should be
                ;; outdented so that their following terms are aligned.
                (+ (if (progn
                         (if (equal sym-after ";")
                             (urweb-move-if
                              (urweb-backward-spaces)
                              (member (urweb-backward-sym) urweb-starters-syms)))
                         (and sym-after (not (looking-at sym-after))))
                       indent-after 0)
                   (current-column))))))))
    (error 0)))


;; maybe `|' should be set to word-syntax in our temp syntax table ?
(defun urweb-current-indentation ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t|")
    (current-column)))


(defun urweb-find-matching-starter (syms &optional prec)
  (let (sym)
    (ignore-errors
      (while
	  (progn (urweb-backward-sexp prec)
		 (setq sym (save-excursion (urweb-forward-sym)))
		 (not (or (member sym syms) (bobp)))))
      (if (member sym syms) sym))))

(defun urweb-skip-siblings ()
  (while (and (not (bobp)) (urweb-backward-arg))
    (urweb-find-matching-starter urweb-starters-syms)))

(defun urweb-beginning-of-defun ()
  (let ((sym (urweb-find-matching-starter urweb-starters-syms)))
    (if (member sym '("fun" "and" "functor" "signature" "structure"
		      "datatype"))
	(save-excursion (urweb-forward-sym) (urweb-forward-spaces)
			(urweb-forward-sym))
      ;; We're inside a "non function declaration": let's skip all other
      ;; declarations that we find at the same level and try again.
      (urweb-skip-siblings)
      ;; Obviously, let's not try again if we're at bobp.
      (unless (bobp) (urweb-beginning-of-defun)))))

(defcustom urweb-max-name-components 3
  "Maximum number of components to use for the current function name."
  :group 'urweb
  :type 'integer)

(defun urweb-current-fun-name ()
  (save-excursion
    (let ((count urweb-max-name-components)
	  fullname name)
      (end-of-line)
      (while (and (> count 0)
		  (setq name (urweb-beginning-of-defun)))
	(decf count)
	(setq fullname (if fullname (concat name "." fullname) name))
	;; Skip all other declarations that we find at the same level.
	(urweb-skip-siblings))
      fullname)))

(defun urweb-get-proj-dir (bfn)
  (locate-dominating-file
   bfn
   (lambda (dir)
     (some (lambda (f) (s-suffix? ".urp" f))
           (if (f-dir? dir)
               (directory-files dir)
             (list '(dir)))))))

(defun urweb-get-info ()
  (interactive)
  (let*
      ((row (line-number-at-pos))
       (col (evil-column))
       (bfn (buffer-file-name))
       (proj-dir (urweb-get-proj-dir bfn))
       (filename (file-relative-name bfn proj-dir))
       (loc (concat filename ":" (number-to-string row) ":" (number-to-string col)))
       )
    (require 's)
    (require 'f)
    (require 'simple)
    (message (let
                 ((default-directory proj-dir))
               (shell-command-to-string (concat "urweb -getInfo " loc)))))
  )

(provide 'urweb-mode)

;;; urweb-mode.el ends here
