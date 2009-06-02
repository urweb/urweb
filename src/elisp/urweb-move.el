;;; urweb-move.el --- Buffer navigation functions for urweb-mode

;; Based on urweb-mode:
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

(eval-when-compile (require 'cl))
(require 'urweb-util)
(require 'urweb-defs)

(defsyntax urweb-internal-syntax-table
  '((?_  . "w")
    (?'  . "w")
    (?.  . "w"))
  "Syntax table used for internal urweb-mode operation."
  :copy urweb-mode-syntax-table)

;;; 
;;; various macros
;;; 

(defmacro urweb-with-ist (&rest r)
  (let ((ost-sym (make-symbol "oldtable")))
    `(let ((,ost-sym (syntax-table))
	   (case-fold-search nil)
	   (parse-sexp-lookup-properties t)
	   (parse-sexp-ignore-comments t))
       (unwind-protect
	   (progn (set-syntax-table urweb-internal-syntax-table) . ,r)
	 (set-syntax-table ,ost-sym)))))
(def-edebug-spec urweb-with-ist t)

(defmacro urweb-move-if (&rest body)
  (let ((pt-sym (make-symbol "point"))
	(res-sym (make-symbol "result")))
    `(let ((,pt-sym (point))
	   (,res-sym ,(cons 'progn body)))
       (unless ,res-sym (goto-char ,pt-sym))
       ,res-sym)))
(def-edebug-spec urweb-move-if t)

(defmacro urweb-point-after (&rest body)
  `(save-excursion
     ,@body
     (point)))
(def-edebug-spec urweb-point-after t)

;;

(defvar urweb-op-prec
  (urweb-preproc-alist
   '((("UNION" "INTERSECT" "EXCEPT") . 0)
     (("AND" "OR") . 1)
     ((">=" "<>" "<=" "=") . 4)
     (("+" "-" "^") . 6)
     (("*" "%") . 7)
     (("NOT") 9)))
  "Alist of Ur/Web infix operators and their precedence.")

(defconst urweb-syntax-prec
  (urweb-preproc-alist
   `(("," . 20)
     (("=>" "d=" "=of") . (65 . 40))
     ("|" . (47 . 30))
     (("case" "of" "fn") . 45)
     (("if" "then" "else" ) . 50)
     (";" . 53)
     (("<-") . 55)
     ("||" . 70)
     ("&&" . 80)
     ((":" ":>") . 90)
     ("->" . 95)
     ("with" . 100)
     (,(cons "end" urweb-begin-syms) . 10000)))
  "Alist of pseudo-precedence of syntactic elements.")

(defun urweb-op-prec (op dir)
  "Return the precedence of OP or nil if it's not an infix.
DIR should be set to BACK if you want to precedence w.r.t the left side
    and to FORW for the precedence w.r.t the right side.
This assumes that we are `looking-at' the OP."
  (when op
    (let ((sprec (cdr (assoc op urweb-syntax-prec))))
      (cond
       ((consp sprec) (if (eq dir 'back) (car sprec) (cdr sprec)))
       (sprec sprec)
       (t
	(let ((prec (cdr (assoc op urweb-op-prec))))
	  (when prec (+ prec 100))))))))

;;

(defun urweb-forward-spaces () (forward-comment 100000))
(defun urweb-backward-spaces () (forward-comment -100000))


;;
;; moving forward around matching symbols
;;

(defun urweb-looking-back-at (re)
  (save-excursion
    (when (= 0 (skip-syntax-backward "w_")) (backward-char))
    (looking-at re)))

(defun urweb-find-match-forward (this match)
  "Only works for word matches."
  (let ((level 1)
	(forward-sexp-function nil)
	(either (concat this "\\|" match)))
    (while (> level 0)
      (forward-sexp 1)
      (while (not (or (eobp) (urweb-looking-back-at either)))
	(condition-case () (forward-sexp 1) (error (forward-char 1))))
      (setq level
	    (cond
	     ((and (eobp) (> level 1)) (error "Unbalanced"))
	     ((urweb-looking-back-at this) (1+ level))
	     ((urweb-looking-back-at match) (1- level))
	     (t (error "Unbalanced")))))
    t))

(defun urweb-find-match-backward (this match)
  (let ((level 1)
	(forward-sexp-function nil)
	(either (concat this "\\|" match)))
    (while (> level 0)
      (backward-sexp 1)
      (while (not (or (bobp) (looking-at either)))
	(condition-case () (backward-sexp 1) (error (backward-char 1))))
      (setq level
	    (cond
	     ((and (bobp) (> level 1)) (error "Unbalanced"))
	     ((looking-at this) (1+ level))
	     ((looking-at match) (1- level))
	     (t (error "Unbalanced")))))
    t))

;;; 
;;; read a symbol, including the special "op <sym>" case
;;; 

(defmacro urweb-move-read (&rest body)
  (let ((pt-sym (make-symbol "point")))
    `(let ((,pt-sym (point)))
       ,@body
       (when (/= (point) ,pt-sym)
	 (buffer-substring-no-properties (point) ,pt-sym)))))
(def-edebug-spec urweb-move-read t)

(defun urweb-poly-equal-p ()
  (< (urweb-point-after (re-search-backward urweb-=-starter-re nil 'move))
     (urweb-point-after (re-search-backward "=" nil 'move))))

(defun urweb-nested-of-p ()
  (< (urweb-point-after
      (re-search-backward urweb-non-nested-of-starter-re nil 'move))
     (urweb-point-after (re-search-backward "\\<case\\>" nil 'move))))

(defun urweb-forward-sym-1 ()
  (or (/= 0 (skip-syntax-forward "'w_"))
      (/= 0 (skip-syntax-forward ".'"))))
(defun urweb-forward-sym ()
  (interactive)
  (let ((sym (urweb-move-read (urweb-forward-sym-1))))
    (cond
     ((equal "op" sym)
      (urweb-forward-spaces)
      (concat "op " (or (urweb-move-read (urweb-forward-sym-1)) "")))
     ((equal sym "=")
      (save-excursion
	(urweb-backward-sym-1)
	(if (urweb-poly-equal-p) "=" "d=")))
     ((equal sym "of")
      (save-excursion
	(urweb-backward-sym-1)
	(if (urweb-nested-of-p) "of" "=of")))
     ;; ((equal sym "datatype")
     ;;  (save-excursion
     ;; 	(urweb-backward-sym-1)
     ;; 	(urweb-backward-spaces)
     ;; 	(if (eq (preceding-char) ?=) "=datatype" sym)))
     (t sym))))

(defun urweb-backward-sym-1 ()
  (or (/= 0 (skip-syntax-backward ".'"))
      (/= 0 (skip-syntax-backward "'w_"))))
(defun urweb-backward-sym ()
  (interactive)
  (let ((sym (urweb-move-read (urweb-backward-sym-1))))
    (let ((result
           (when sym
             ;; FIXME: what should we do if `sym' = "op" ?
             (let ((point (point)))
               (urweb-backward-spaces)
               (if (equal "op" (urweb-move-read (urweb-backward-sym-1)))
                   (concat "op " sym)
                 (goto-char point)
                 (cond
                  ((string= sym "=") (if (urweb-poly-equal-p) "=" "d="))
                  ((string= sym "of") (if (urweb-nested-of-p) "of" "=of"))
                  ;; ((string= sym "datatype")
                  ;;  (save-excursion (urweb-backward-spaces)
                  ;; 		    (if (eq (preceding-char) ?=) "=datatype" sym)))
                  (t sym)))))))
      (if (looking-at ">")
          (substring result 1 nil)
        result))))
;;       (if (save-excursion (backward-char 5) (looking-at "</xml>"))
;;           (progn
;;             (backward-char 5)
;;             (urweb-tag-matcher)
;;             (backward-char)
;;             (urweb-backward-sym))
;;         result))))

(defun urweb-tag-matcher ()
  "Seek back to a matching opener tag"
  (let ((depth 0)
        (done nil))
    (while (and (not done) (search-backward ">" nil t))
      (cond
       ((save-excursion (backward-char 1) (looking-at " "))
        nil)
       ((save-excursion (backward-char 1) (looking-at "/"))
        (when (not (re-search-backward "<[^ =]" nil t))
          (setq done t)))
       (t
        (if (re-search-backward "<[^ =]" nil t)
            (if (looking-at "</")
                (incf depth)
              (if (= depth 0)
                  (setq done t)
                (decf depth)))
          (setq done t)))))))

(defun urweb-backward-sexp (prec)
  "Move one sexp backward if possible, or one char else.
Returns t if the move indeed moved through one sexp and nil if not.
PREC is the precedence currently looked for."
  (let ((result (let ((parse-sexp-lookup-properties t)
                      (parse-sexp-ignore-comments t))
                  (urweb-backward-spaces)
                  (let* ((op (urweb-backward-sym))
                         (op-prec (urweb-op-prec op 'back))
                         match)
                  (cond
                   ((not op)
                    (let ((point (point)))
                      (ignore-errors (let ((forward-sexp-function nil)) (backward-sexp 1)))
                      (if (/= point (point)) t (ignore-errors (backward-char 1)) nil)))
                   ;; stop as soon as precedence is smaller than `prec'
                   ((and prec op-prec (>= prec op-prec)) nil)
                   ;; special rules for nested constructs like if..then..else
                   ((and (or (not prec) (and prec op-prec))
                         (setq match (second (assoc op urweb-close-paren))))
                    (urweb-find-match-backward (concat "\\<" op "\\>") match))
                   ;; don't back over open-parens
                   ((assoc op urweb-open-paren) nil)
                   ;; infix ops precedence
                   ((and prec op-prec) (< prec op-prec))
                   ;; [ prec = nil ]  a new operator, let's skip the sexps until the next
                   (op-prec (while (urweb-move-if (urweb-backward-sexp op-prec))) t)
                   ;; special symbols indicating we're getting out of a nesting level
                   ((string-match urweb-sexp-head-symbols-re op) nil)
                   ;; if the op was not alphanum, then we still have to do the backward-sexp
                   ;; this reproduces the usual backward-sexp, but it might be bogus
                   ;; in this case since !@$% is a perfectly fine symbol
                   (t t))))))
    (if (save-excursion (backward-char 5) (looking-at "</xml>"))
      (progn
        (backward-char 5)
        (urweb-tag-matcher)
        (backward-char)
        (urweb-backward-sexp prec))
      result)))

(defun urweb-forward-sexp (prec)
  "Moves one sexp forward if possible, or one char else.
Returns T if the move indeed moved through one sexp and NIL if not."
  (let ((parse-sexp-lookup-properties t)
	(parse-sexp-ignore-comments t))
    (urweb-forward-spaces)
    (let* ((op (urweb-forward-sym))
	   (op-prec (urweb-op-prec op 'forw))
	   match)
      (cond
       ((not op)
	(let ((point (point)))
	  (ignore-errors (let ((forward-sexp-function nil)) (forward-sexp 1)))
	  (if (/= point (point)) t (forward-char 1) nil)))
       ;; stop as soon as precedence is smaller than `prec'
       ((and prec op-prec (>= prec op-prec)) nil)
       ;; special rules for nested constructs like if..then..else
       ((and (or (not prec) (and prec op-prec))
	     (setq match (cdr (assoc op urweb-open-paren))))
	(urweb-find-match-forward (first match) (second match)))
       ;; don't forw over close-parens
       ((assoc op urweb-close-paren) nil)
       ;; infix ops precedence
       ((and prec op-prec) (< prec op-prec))
       ;; [ prec = nil ]  a new operator, let's skip the sexps until the next
       (op-prec (while (urweb-move-if (urweb-forward-sexp op-prec))) t)
       ;; special symbols indicating we're getting out of a nesting level
       ((string-match urweb-sexp-head-symbols-re op) nil)
       ;; if the op was not alphanum, then we still have to do the backward-sexp
       ;; this reproduces the usual backward-sexp, but it might be bogus
       ;; in this case since !@$% is a perfectly fine symbol
       (t t))))) ;(or (string-match "\\sw" op) (urweb-backward-sexp prec))

(defun urweb-in-word-p ()
  (and (eq ?w (char-syntax (or (char-before) ? )))
       (eq ?w (char-syntax (or (char-after) ? )))))

(defun urweb-user-backward-sexp (&optional count)
  "Like `backward-sexp' but tailored to the Ur/Web syntax."
  (interactive "p")
  (unless count (setq count 1))
  (urweb-with-ist
   (let ((point (point)))
     (if (< count 0) (urweb-user-forward-sexp (- count))
       (when (urweb-in-word-p) (forward-word 1))
       (dotimes (i count)
	 (unless (urweb-backward-sexp nil)
	   (goto-char point)
	   (error "Containing expression ends prematurely")))))))


(defun urweb-user-forward-sexp (&optional count)
  "Like `forward-sexp' but tailored to the Ur/Web syntax."
  (interactive "p")
  (unless count (setq count 1))
  (urweb-with-ist
   (let ((point (point)))
     (if (< count 0) (urweb-user-backward-sexp (- count))
       (when (urweb-in-word-p) (backward-word 1))
       (dotimes (i count)
	 (unless (urweb-forward-sexp nil)
	   (goto-char point)
	   (error "Containing expression ends prematurely")))))))

;;(defun urweb-forward-thing ()
;;  (if (= ?w (char-syntax (char-after))) (forward-word 1) (forward-char 1)))

(defun urweb-backward-arg () (interactive) (urweb-backward-sexp 1000))
(defun urweb-forward-arg () (interactive) (urweb-forward-sexp 1000))


(provide 'urweb-move)

;;; urweb-move.el ends here
