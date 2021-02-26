;;; urweb-defs.el --- Various definitions for urweb-mode

;; Based on sml-mode:
;; Copyright (C) 1999,2000,2003  Stefan Monnier <monnier@cs.yale.edu>
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

(eval-when-compile (require 'cl))
(require 'urweb-util)


(defgroup urweb ()
  "Editing Ur/Web code."
  :group 'languages)

(defvar urweb-outline-regexp
  ;; `st' and `si' are to match structure and signature.
  "\\|s[ti]\\|[ \t]*\\(let[ \t]+\\)?\\(fun\\|and\\)\\>"
  "Regexp matching a major heading.
This actually can't work without extending `outline-minor-mode' with the
notion of \"the end of an outline\".")

;;; 
;;; Internal defines
;;; 

(defmap urweb-mode-map
  ;; smarter cursor movement
  '(("\C-c\C-i"	. urweb-mode-info))
  "The keymap used in `urweb-mode'."
  ;; :inherit urweb-bindings
  :group 'urweb)

(defsyntax urweb-mode-syntax-table
  `((?\*   . ,(if urweb-builtin-nested-comments-flag ". 23n" ". 23"))
    (?\(   . "()1")
    (?\)   . ")(4")
    ("._'" . "_")
    (",;"  . ".")
    ;; `!' is not really a prefix-char, oh well!
    ("~#!" . "'")
    ("%&$+-/:<=>?@`^|"	 . "."))
  "The syntax table used in `urweb-mode'.")


(easy-menu-define urweb-mode-menu urweb-mode-map "Menu used in `urweb-mode'."
  '("Ur/Web"
    ["Ur/Web mode help (brief)"       describe-mode t]
    ["Ur/Web mode *info*"             urweb-mode-info t]
    ))

;; Make's sure they appear in the menu bar when urweb-mode-map is active.
;; On the hook for XEmacs only -- see easy-menu-add in auc-menu.el.
;; (defun urweb-mode-menu-bar ()
;;   "Make sure menus appear in the menu bar as well as under mouse 3."
;;   (and (eq major-mode 'urweb-mode)
;;        (easy-menu-add urweb-mode-menu urweb-mode-map)))
;; (add-hook 'urweb-mode-hook 'urweb-mode-menu-bar)

;;
;; regexps
;;

(defun urweb-syms-re (&rest syms)
  (concat "\\<" (regexp-opt (flatten syms) t) "\\>"))

;;

(defconst urweb-module-head-syms
  '("signature" "structure" "functor"))


(defconst urweb-begin-syms
  '("let" "struct" "sig")
  "Symbols matching the `end' symbol.")

(defconst urweb-begin-syms-re
  (urweb-syms-re urweb-begin-syms)
  "Symbols matching the `end' symbol.")

;; (defconst urweb-user-begin-symbols-re
;;   (urweb-syms-re "let" "abstype" "local" "struct" "sig" "in" "with")
;;   "Symbols matching (loosely) the `end' symbol.")

(defconst urweb-sexp-head-symbols-re
  (urweb-syms-re "let" "struct" "sig" "in" "with"
                 "if" "then" "else" "case" "of" "fn" "fun" "val" "and"
                 "datatype" "type" "open" "include"
                 urweb-module-head-syms
                 "con" "map" "where" "extern" "constraint" "constraints"
                 "table" "sequence" "ensure_index" "class" "cookie" "style" "task" "policy")
  "Symbols starting an sexp.")

;; (defconst urweb-not-arg-start-re
;;   (urweb-syms-re "in" "of" "end" "andalso")
;;   "Symbols that can't be found at the head of an arg.")

;; (defconst urweb-not-arg-re
;;   (urweb-syms-re "in" "of" "end" "andalso")
;;   "Symbols that should not be confused with an arg.")

(defconst urweb-=-starter-syms
  (list* "|" "val" "fun" "and" "datatype" "con" "type" "class"
	 urweb-module-head-syms)
  "Symbols that can be followed by a `='.")
(defconst urweb-=-starter-re
  (concat "\\S.|\\S.\\|" (urweb-syms-re (cdr urweb-=-starter-syms)))
  "Symbols that can be followed by a `='.")

(defconst urweb-indent-rule
  (urweb-preproc-alist
   `((,urweb-module-head-syms "d=" 0)
     ("if" "else" 0)
     (,urweb-=-starter-syms nil)
     (("case" "datatype" "if" "then" "else"
       "let" "open" "sig" "struct" "type" "val"
       "con" "constraint" "table" "sequence" "ensure_index" "class" "cookie"
       "style" "task" "policy")))))

(defconst urweb-starters-indent-after
  (urweb-syms-re "let" "in" "struct" "sig")
  "Indent after these.")

(defconst urweb-delegate
  (urweb-preproc-alist
   `((("of" "else" "then" "with" "d=") . (not (urweb-bolp)))
     ("in" . t)))
  "Words which might delegate indentation to their parent.")

(defcustom urweb-symbol-indent
  '(("fn" . -3)
    ("of" . 1)
    ("|" . -2)
    ("," . -2)
    (";" . -2)
    ;;("in" . 1)
    ("d=" . 2))
  "Special indentation alist for some symbols.
An entry like (\"in\" . 1) indicates that a line starting with the
symbol `in' should be indented one char further to the right.
This is only used in a few specific cases, so it does not work
for all symbols and in all lines starting with the given symbol."
  :group 'urweb
  :type '(repeat (cons string integer)))

(defconst urweb-open-paren
  (urweb-preproc-alist
   `((,(list* "in" urweb-begin-syms) ,urweb-begin-syms-re "\\<end\\>")))
  "Symbols that should behave somewhat like opening parens.")

(defconst urweb-close-paren
  `(("in" "\\<let\\>")
    ("end" ,urweb-begin-syms-re)
    ("then" "\\<if\\>")
    ("else" "\\<if\\>" (urweb-bolp))
    ("of" "\\<case\\>")
    ("</xml>" "<xml>")
    ("d=" nil))
  "Symbols that should behave somewhat like close parens.")

(defconst urweb-agglomerate-re "\\<else[ \t]+if\\>"
  "Regexp of compound symbols (pairs of symbols to be considered as one).")

(defconst urweb-non-nested-of-starter-re
  (urweb-syms-re "datatype")
  "Symbols that can introduce an `of' that shouldn't behave like a paren.")

(defconst urweb-starters-syms
  (append urweb-module-head-syms
	  '("datatype" "fun"
	    "open" "type" "val" "and"
	    "con" "constraint" "table" "sequence" "ensure_index" "class" "cookie"
            "style" "task" "policy"))
  "The starters of new expressions.")

(defconst urweb-exptrail-syms
  '("if" "then" "else" "case" "of" "fn" "with" "map"))

(defconst urweb-pipeheads
   '("|" "of" "fun" "fn" "and" "datatype")
   "A `|' corresponds to one of these.")


(provide 'urweb-defs)

;;; urweb-defs.el ends here
