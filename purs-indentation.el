;;; purs-indentation.el -- indentation module for PureScript Mode

;; Copyright (C) 2009  Kristof Bastiaensen

;; Author: Kristof Bastiaensen <kristof.bastiaensen@vleeuwen.org>

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Installation:
;;
;; To turn indentation on for all PureScript buffers under PureScript mode
;; <http://www.purescript.org/purescript-mode/> add this to .emacs:
;;
;;    (add-hook purs-mode-hook 'purs-indentation-mode)
;;
;; Otherwise, call `purs-indentation-mode'.

;;; Code:

(require 'syntax)
(require 'cl-lib)

(defvar delete-active-region)

;; Dynamically scoped variables.
(defvar following-token)
(defvar current-token)
(defvar left-indent)
(defvar starter-indent)
(defvar current-indent)
(defvar layout-indent)
(defvar parse-line-number)
(defvar possible-indentations)
(defvar indentation-point)

(defgroup purs-indentation nil
  "PureScript indentation."
  :link '(custom-manual "(purs-mode)Indentation")
  :group 'purs
  :prefix "purs-indentation-")

(defcustom purs-indentation-cycle-warn t
  "Warn before moving to the leftmost indentation, if you tab at the rightmost one."
  :type 'boolean
  :group 'purs-indentation)

(defcustom purs-indentation-delete-backward-indentation t
  "Delete backward removes indentation."
  :type 'boolean
  :group 'purs-indentation)

(defcustom purs-indentation-delete-backward-jump-line nil
  "Delete backward jumps to the previous line when removing last indentation."
  :type 'boolean
  :group 'purs-indentation)

(defcustom purs-indentation-delete-indentation t
  "Delete removes indentation."
  :type 'boolean
  :group 'purs-indentation)

(defcustom purs-indentation-layout-offset 2
  "Extra indentation to add before expressions in a purs layout list."
  :type 'integer
  :group 'purs-indentation)

(defcustom purs-indentation-starter-offset 1
  "Extra indentation after an opening keyword (e.g. let)."
  :type 'integer
  :group 'purs-indentation)

(defcustom purs-indentation-left-offset 2
  "Extra indentation after an indentation to the left (e.g. after do)."
  :type 'integer
  :group 'purs-indentation)

(defcustom  purs-indentation-ifte-offset 2
  "Extra indentation after the keywords `if' `then' or `else'."
  :type 'integer
  :group 'purs-indentation)

(defcustom purs-indentation-where-pre-offset 2
  "Extra indentation before the keyword `where'."
  :type 'integer
  :group 'purs-indentation)

(defcustom purs-indentation-where-post-offset 2
  "Extra indentation after the keyword `where'."
  :type 'integer
  :group 'purs-indentation)

;; Avoid a global bogus definition (which the original run-time
;; `defun' made), and support Emacs 21 without the syntax.el add-on.
(eval-when-compile
  (unless (fboundp 'syntax-ppss)
    (defsubst syntax-ppss (&rest pos)
      (parse-partial-sexp (point-min) (or pos (point))))))

;; Used internally
(defvar purs-indent-last-position)

(defun purs-current-column ()
  "Compute current column according to purs syntax rules,
  correctly ignoring composition."
  (save-excursion
    (let ((start (point))
          (cc 0))
      (beginning-of-line)
      (while (< (point) start)
        (if (= (char-after) ?\t)
            (setq cc (* 8 (+ 1 (/ cc 8))))
          (cl-incf cc))
        (forward-char))
      cc)))

(defun purs-indentation-auto-fill-function ()
  (when (> (purs-current-column) fill-column)
    (while (> (purs-current-column) fill-column)
      (skip-syntax-backward "-")
      (skip-syntax-backward "^-"))
    (let ((auto-fill-function nil)
          (indent (car (last (purs-indentation-find-indentations)))))
      (newline)
      (indent-to indent)
      (end-of-line))))

(defun purs-indentation-reindent (col)
  (beginning-of-line)
  (delete-region (point)
                 (progn
                   (skip-syntax-forward "-")
                   (point)))
  (indent-to col))

(defun purs-indentation-current-indentation ()
  (current-indentation))

(defun purs-newline-and-indent ()
  (interactive)
  (let* ((cc (purs-current-column))
         (ci (purs-indentation-current-indentation))
         (indentations (purs-indentation-find-indentations)))
    (skip-syntax-forward "-")
    (if (prog1 (and (eolp)
                    (not (= (purs-current-column) ci)))
          (delete-horizontal-space)
          (newline))
        (purs-indentation-reindent
         (max (purs-indentation-butlast indentations)
              (purs-indentation-matching-indentation
               ci indentations)))
      (purs-indentation-reindent (purs-indentation-matching-indentation
                                  cc indentations)))))

(defun purs-indentation-one-indentation (col indentations)
  (let* ((last-pair (last indentations)))
    (cond ((null indentations)
           col)
          ((null (cdr indentations))
           (car indentations))
          ((<= col (car last-pair))
           col)
          (t (car last-pair)))))

(defun purs-indentation-butlast (indentations)
  (when (consp (cdr indentations))
    (while (cddr indentations)
      (setq indentations (cdr indentations))))
  (car indentations))

(defun purs-indentation-next-indentation (col indentations)
  "Find the lefmost indentation which is greater than COL."
  (catch 'return
    (while indentations
      (if (or (< col (car indentations))
              (null (cdr indentations)))
          (throw 'return (car indentations))
        (setq indentations (cdr indentations))))
    col))

(defun purs-indentation-previous-indentation (col indentations)
  "Find the rightmost indentation which is less than COL."
  (and indentations
       (> col (car indentations))
       (catch 'return
         (while indentations
           (if (or (null (cdr indentations))
                   (<= col (cadr indentations)))
               (throw 'return (car indentations))
             (setq indentations (cdr indentations))))
         col)))

(defun purs-indentation-matching-indentation (col indentations)
  "Find the leftmost indentation which is greater than or equal to COL."
  (catch 'return
    (while indentations
      (if (or (<= col (car indentations))
              (null (cdr indentations)))
          (throw 'return (car indentations))
        (setq indentations (cdr indentations))))
    col))

(defun purs-indentation-indent-line ()
  (when (save-excursion
          (beginning-of-line)
          (not (nth 8 (syntax-ppss))))
    (let ((ci (purs-indentation-current-indentation))
          (start-column (purs-current-column)))
      (cond ((> (purs-current-column) ci)
             (save-excursion
               (move-to-column ci)
               (purs-indentation-reindent
                (purs-indentation-one-indentation
                 ci (purs-indentation-find-indentations)))))

            ((= (purs-current-column) ci)
             (purs-indentation-reindent
              (purs-indentation-next-indentation
               ci (purs-indentation-find-indentations))))

            (t (move-to-column ci)
               (purs-indentation-reindent
                (purs-indentation-matching-indentation
                 ci (purs-indentation-find-indentations)))))
      (cond ((not (= (purs-current-column) start-column))
             (setq purs-indent-last-position nil))
            ((not purs-indentation-cycle-warn)
             (purs-indentation-reindent
              (purs-indentation-next-indentation
               -1
               (purs-indentation-find-indentations))))
            ((not (equal (point) purs-indent-last-position))
             (message "Press TAB again to go to the leftmost indentation")
             (setq purs-indent-last-position (point)))
            (t
             (purs-indentation-reindent
              (purs-indentation-next-indentation
               -1
               (purs-indentation-find-indentations))))))))

(defun purs-indentation-delete-backward-char (n)
  (interactive "p")
  (cond
   ((and (use-region-p)
         delete-active-region
         (not (= (point) (mark))))
    (delete-region (mark) (point)))
   ((or (= (purs-current-column) 0)
        (> (purs-current-column) (purs-indentation-current-indentation))
        (nth 8 (syntax-ppss)))
    (delete-char (- n)))
   (purs-indentation-delete-backward-indentation
    (let* ((ci (purs-indentation-current-indentation))
           (float-pi (purs-indentation-previous-indentation
                ci (purs-indentation-find-indentations))))
      (save-excursion
        (cond (float-pi
               (move-to-column float-pi)
               (delete-region (point)
                              (progn (move-to-column ci)
                                     (point))))
              (t
               (if (not purs-indentation-delete-backward-jump-line)
                   (delete-char (- 1))
                 (beginning-of-line)
                 (delete-region (max (point-min) (- (point) 1))
                                (progn (move-to-column ci)
                                       (point)))))))))
   (t (delete-char (- n)))))

(defun purs-indentation-delete-char (n)
  (interactive "p")
  (cond
   ((and delete-selection-mode
         mark-active
         (not (= (point) (mark))))
    (delete-region (mark) (point)))
   ((or (eolp)
        (>= (purs-current-column) (purs-indentation-current-indentation))
        (nth 8 (syntax-ppss)))
    (delete-char n))
   (purs-indentation-delete-indentation
    (let* ((ci (purs-indentation-current-indentation))
           (float-pi (purs-indentation-previous-indentation
                      ci (purs-indentation-find-indentations))))
      (save-excursion
        (if (and float-pi (> float-pi (purs-current-column)))
            (move-to-column float-pi))
        (delete-region (point)
                       (progn (move-to-column ci)
                              (point))))))
   (t (delete-char (- n)))))

(defun purs-indentation-goto-least-indentation ()
  (beginning-of-line)
  (catch 'return
    (while (not (bobp))
      (forward-comment (- (buffer-size)))
      (beginning-of-line)
      (let ((ps (nth 8 (syntax-ppss))))
        (when ps ;; inside comment or string
          (goto-char ps)))
      (when (= 0 (purs-indentation-current-indentation))
        (throw 'return nil))))
  (beginning-of-line)
  (when (bobp)
    (forward-comment (buffer-size))))

(defun purs-indentation-parse-to-indentations ()
  (save-excursion
    (skip-syntax-forward "-")
    (let ((indentation-point (point))
          (layout-indent 0)
          (parse-line-number 0)
          (current-indent purs-indentation-layout-offset)
          (starter-indent purs-indentation-layout-offset)
          (left-indent purs-indentation-layout-offset)
          (case-fold-search nil)
          current-token
          following-token
          possible-indentations)
      (purs-indentation-goto-least-indentation)
      (if (<= indentation-point (point))
          (purs-indentation-first-indentation)
        (setq current-token (purs-indentation-peek-token))
        (catch 'parse-end
          (purs-indentation-toplevel))
        possible-indentations))))

(defun purs-indentation-first-indentation ()
  '(0))

(defun purs-indentation-find-indentations ()
  (let ((ppss (syntax-ppss)))
    (cond
     ((nth 3 ppss)
      (purs-indentation-first-indentation))
     ((nth 4 ppss)
      (if (save-excursion
            (and (skip-syntax-forward "-")
                 (eolp)
                 (not (> (forward-line 1) 0))
                 (not (nth 4 (syntax-ppss)))))
          (purs-indentation-parse-to-indentations)
        (purs-indentation-first-indentation)))
     (t
      (purs-indentation-parse-to-indentations)))))

(defconst purs-indentation-unicode-tokens
  '(("→" . "->")     ;; #x2192 RIGHTWARDS ARROW
    ("∷" . "::")     ;; #x2237 PROPORTION
    ("←" . "<-")     ;; #x2190 LEFTWARDS ARROW
    ("⇒" . "=>")     ;; #x21D2 RIGHTWARDS DOUBLE ARROW
    ("∀" . "forall") ;; #x2200 FOR ALL
    ("↢" . "-<")     ;; #x2919 LEFTWARDS ARROW-TAIL
    ("↣" . ">-")     ;; #x291A RIGHTWARDS ARROW-TAIL
    ("⤛" . "-<<")    ;; #x291B LEFTWARDS DOUBLE ARROW-TAIL
    ("⤜" . ">>-")    ;; #x291C RIGHTWARDS DOUBLE ARROW-TAIL
    ("★" . "*"))     ;; #x2605 BLACK STAR
  "Translation dictionary from UnicodeSyntax tokens to their ASCII representation.")

(defconst purs-indentation-toplevel-list
  '(("module" . purs-indentation-module)
    ("data" . (lambda () (purs-indentation-statement-right #'purs-indentation-data)))
    ("type" . (lambda () (purs-indentation-statement-right #'purs-indentation-data)))
    ("newtype" . (lambda () (purs-indentation-statement-right #'purs-indentation-data)))
    ("class" . purs-indentation-class-declaration)
    ("instance" . purs-indentation-class-declaration)
    ("else" . purs-indentation-else-instance)))

(defconst purs-indentation-type-list
  '(("::"    . (lambda () (purs-indentation-with-starter
                           (lambda () (purs-indentation-separated #'purs-indentation-type "->" nil)) nil)))
    ("("     . (lambda () (purs-indentation-list #'purs-indentation-type
                                                    ")" "," nil)))
    ("["     . (lambda () (purs-indentation-list #'purs-indentation-type
                                                    "]" "," nil)))
    ("{"     . (lambda () (purs-indentation-list #'purs-indentation-type
                                                    "}" "," "|")))))

(defconst purs-indentation-expression-list
  '(("data" . purs-indentation-data)
    ("type" . purs-indentation-data)
    ("newtype" . purs-indentation-data)
    ("if"    . (lambda () (purs-indentation-phrase
                           '(purs-indentation-expression
                             "then" purs-indentation-expression
                             "else" purs-indentation-expression))))
    ("let"   . (lambda () (purs-indentation-phrase
                           '(purs-indentation-declaration-layout
                             "in" purs-indentation-expression))))
    ("do"    . (lambda () (purs-indentation-with-starter
                           #'purs-indentation-expression-layout nil)))
    ("ado"   . (lambda () (purs-indentation-with-starter
                           #'purs-indentation-expression-layout nil)))
    ("in"    . (lambda () (purs-indentation-with-starter
                           #'purs-indentation-expression nil)))
    ("mdo"   . (lambda () (purs-indentation-with-starter
                           #'purs-indentation-expression-layout nil)))
    ("rec"   . (lambda () (purs-indentation-with-starter
                           #'purs-indentation-expression-layout nil)))
    ("case"  . (lambda () (purs-indentation-phrase
                           `(,(lambda () (purs-indentation-separated #'purs-indentation-expression "," nil))
                             "of" purs-indentation-case-layout))))
    ("\\"    . (lambda () (purs-indentation-with-starter
                           #'purs-indentation-lambda-maybe-lambdacase nil)))
    ("proc"  . (lambda () (purs-indentation-phrase
                           '(purs-indentation-expression
                             "->" purs-indentation-expression))))
    ("where" . (lambda () (purs-indentation-with-starter
                           #'purs-indentation-declaration-layout nil t)))
    ("::"    . (lambda () (purs-indentation-with-starter
                           (lambda () (purs-indentation-separated #'purs-indentation-type "->" nil)) nil)))
    ("="     . (lambda () (purs-indentation-statement-right #'purs-indentation-expression)))
    ("<-"    . (lambda () (purs-indentation-statement-right #'purs-indentation-expression)))
    ("("     . (lambda () (purs-indentation-list #'purs-indentation-expression
                                                    ")" '(list "," "->") nil)))
    ("["     . (lambda () (purs-indentation-list #'purs-indentation-expression
                                                    "]" "," "|")))
    ("{"     . (lambda () (purs-indentation-list #'purs-indentation-expression
                                                    "}" "," nil)))))

(defun purs-indentation-qualified-do-p (token)
  (and (stringp token)
       (string-match-p "\\.do\\'" token)))

(defun purs-indentation-find-expression-parser (token)
  "Find the entry for TOKEN in `purs-indentation-expression-list'."
  (or (assoc token purs-indentation-expression-list)
      (when (purs-indentation-qualified-do-p token)
        (assoc "do" purs-indentation-expression-list))))

(defun purs-indentation-expression-layout ()
  (purs-indentation-layout #'purs-indentation-expression))

(defun purs-indentation-declaration-layout ()
  (purs-indentation-layout #'purs-indentation-declaration))

(defun purs-indentation-case-layout ()
  (purs-indentation-layout #'purs-indentation-case))

;; After a lambda (backslash) there are two possible cases:
;;   - the new lambdacase expression, that can be recognized by the
;;     next token being "case",
;;   - or simply an anonymous function definition in the form of
;;     "expression -> expression".
(defun purs-indentation-lambda-maybe-lambdacase ()
  (if (string= current-token "case")
      (purs-indentation-with-starter
       #'purs-indentation-case-layout nil)
    (purs-indentation-phrase-rest
     '(purs-indentation-expression "->" purs-indentation-expression))))

(defun purs-indentation-fundep ()
  (purs-indentation-with-starter
   (lambda () (purs-indentation-separated
               #'purs-indentation-fundep1 "," nil))
   nil))

(defun purs-indentation-fundep1 ()
  (let ((current-indent (purs-current-column)))
    (while (member current-token '(value "->"))
      (purs-indentation-read-next-token))
    (when (and (eq current-token 'end-tokens)
               (member following-token '(value "->")))
      (purs-indentation-add-indentation current-indent))))

(defun purs-indentation-toplevel ()
  (purs-indentation-layout
   (lambda ()
     (let ((parser (assoc current-token purs-indentation-toplevel-list)))
       (if parser
           (funcall (cdr parser))
         (purs-indentation-declaration))))))

(defun purs-indentation-type ()
  (let ((current-indent (purs-current-column)))
    (catch 'return
      (while t
        (cond
         ((member current-token '(value operator "->"))
          (purs-indentation-read-next-token))

         ((eq current-token 'end-tokens)
          (when (member following-token
                        '(value operator no-following-token
                                "->" "(" "[" "{" "::"))
            (purs-indentation-add-indentation current-indent))
          (throw 'return nil))

         (t (let ((parser (assoc current-token purs-indentation-type-list)))
              (if (not parser)
                  (throw 'return nil)
                (funcall (cdr parser))))))))))

(defun purs-indentation-data ()
  (purs-indentation-with-starter
   (lambda ()
     (when (string= current-token "instance")
       (purs-indentation-read-next-token))
     (purs-indentation-type)
     (cond ((string= current-token "=")
            (purs-indentation-with-starter
             (lambda () (purs-indentation-separated #'purs-indentation-type "|" "deriving"))
             nil))
           ((string= current-token "where")
            (purs-indentation-with-starter
             #'purs-indentation-expression-layout nil))))
   nil))

(defun purs-indentation-class-declaration ()
  (purs-indentation-with-starter
   (lambda ()
     (purs-indentation-type)
     (when (string= current-token "|")
       (purs-indentation-fundep))
     (when (string= current-token "where")
       (purs-indentation-with-starter
        #'purs-indentation-expression-layout nil)))
   nil))

(defun purs-indentation-else-instance ()
  (purs-indentation-with-starter
   (lambda ()
     (purs-indentation-class-declaration)) nil))

(defun purs-indentation-module ()
  (purs-indentation-with-starter
   (lambda ()
     (let ((current-indent (purs-current-column)))
       (purs-indentation-read-next-token)
       (when (string= current-token "(")
         (purs-indentation-list
          #'purs-indentation-module-export
          ")" "," nil))
       (when (eq current-token 'end-tokens)
         (purs-indentation-add-indentation current-indent)
         (throw 'parse-end nil))
       (when (string= current-token "where")
         (purs-indentation-read-next-token)
         (when (eq current-token 'end-tokens)
           (purs-indentation-add-layout-indent)
           (throw 'parse-end nil))
         (purs-indentation-layout #'purs-indentation-toplevel))))
   nil))

(defun purs-indentation-module-export ()
  (cond ((string= current-token "module")
         (let ((current-indent (purs-current-column)))
           (purs-indentation-read-next-token)
           (cond ((eq current-token 'end-tokens)
                  (purs-indentation-add-indentation current-indent))
                 ((eq current-token 'value)
                  (purs-indentation-read-next-token)))))
        (t (purs-indentation-type))))

(defun purs-indentation-list (parser end sep stmt-sep)
  (purs-indentation-with-starter
   `(lambda () (purs-indentation-separated #',parser
                                              ,sep
                                              ,stmt-sep))
   end))

(defun purs-indentation-with-starter (parser end &optional where-expr?)
  (let ((starter-column (purs-current-column))
        (current-indent current-indent)
        (left-indent (if (= (purs-current-column) (purs-indentation-current-indentation))
                         (purs-current-column) left-indent)))
    (purs-indentation-read-next-token)
    (when (eq current-token 'end-tokens)
      (if (equal following-token end)
          (purs-indentation-add-indentation starter-column)
        (if where-expr?
            (purs-indentation-add-where-post-indent left-indent)
          (purs-indentation-add-indentation
           (+ left-indent purs-indentation-left-offset))))
      (throw 'parse-end nil))
    (let* ((current-indent (purs-current-column))
           (starter-indent (min starter-column current-indent))
           (left-indent (if end (+ current-indent purs-indentation-starter-offset)
                          left-indent)))
      (funcall parser)
      (cond ((eq current-token 'end-tokens)
             (when (equal following-token end)
               (purs-indentation-add-indentation starter-indent))
             (when end (throw 'parse-end nil))) ;; add no indentations
            ((equal current-token end)
             (purs-indentation-read-next-token))))))

(defun purs-indentation-case ()
  (purs-indentation-separated #'purs-indentation-expression "," nil)
  (cond ((eq current-token 'end-tokens)
         (purs-indentation-add-indentation current-indent))
        ((string= current-token "|")
         (purs-indentation-with-starter
          (lambda () (purs-indentation-separated #'purs-indentation-case "|" nil))
          nil))
        ((string= current-token "->")
         (purs-indentation-statement-right #'purs-indentation-expression))
        ;; otherwise fallthrough
        ))

(defun purs-indentation-statement-right (parser)
  (purs-indentation-read-next-token)
  (when (eq current-token 'end-tokens)
    (purs-indentation-add-indentation
     (+ left-indent purs-indentation-left-offset))
    (throw 'parse-end nil))
  (let ((current-indent (purs-current-column)))
    (funcall parser)))

(defun purs-indentation-simple-declaration ()
  (purs-indentation-expression)
  (cond ((string= current-token "=")
         (purs-indentation-statement-right #'purs-indentation-expression))
        ((string= current-token "::")
         (purs-indentation-statement-right #'purs-indentation-type))
        ((and (eq current-token 'end-tokens)
              (string= following-token "="))
         (purs-indentation-add-indentation current-indent)
         (throw 'parse-end nil))))

(defun purs-indentation-declaration ()
  (purs-indentation-expression)
  (cond ((string= current-token "|")
         (purs-indentation-with-starter
          (lambda () (purs-indentation-separated #'purs-indentation-expression "," "|"))
          nil))
        ((eq current-token 'end-tokens)
         (when (member following-token '("|" "=" "::" ","))
           (purs-indentation-add-indentation current-indent)
           (throw 'parse-end nil)))))

(defun purs-indentation-layout (parser)
  (purs-indentation-implicit-layout-list parser))

(defun purs-indentation-expression-token (token)
  (member token '("if" "let" "do" "case" "\\" "(" "[" "{" "::"
                  value operator no-following-token)))

(defun purs-indentation-expression ()
  (let ((current-indent (purs-current-column)))
    (catch 'return
      (while t
        (cond
         ((memq current-token '(value operator))
          (purs-indentation-read-next-token))

         ((eq current-token 'end-tokens)
          (cond ((string= following-token "where")
                 (purs-indentation-add-where-pre-indent))
                ((purs-indentation-expression-token following-token)
                 (purs-indentation-add-indentation
                  current-indent)))
          (throw 'return nil))

         (t (let ((parser (purs-indentation-find-expression-parser current-token)))
              (when (null parser)
                (throw 'return nil))
              (funcall (cdr parser))
              (when (and (eq current-token 'end-tokens)
                         (string= (car parser) "let")
                         (= purs-indentation-layout-offset current-indent)
                         (purs-indentation-expression-token following-token))
                ;; inside a layout, after a let construct
                (purs-indentation-add-layout-indent)
                (throw 'parse-end nil))
              (unless (member (car parser) '("(" "[" "{" "do" "case"))
                (throw 'return nil)))))))))

(defun purs-indentation-test-indentations ()
  (interactive)
  (let ((indentations (save-excursion (purs-indentation-find-indentations)))
        (str "")
        (pos 0))
    (while indentations
      (when (>= (car indentations) pos)
        (setq str (concat str (make-string (- (car indentations) pos) ?\ )
                          "|"))
        (setq pos (+ 1 (car indentations))))
      (setq indentations (cdr indentations)))
    (end-of-line)
    (newline)
    (insert str)))

(defun purs-indentation-separated (parser separator stmt-separator)
  (catch 'return
    (while t
      (funcall parser)
      (cond ((if (listp separator) (member current-token separator) (equal current-token separator))
             (purs-indentation-at-separator))

            ((equal current-token stmt-separator)
             (setq starter-indent (purs-current-column))
             (purs-indentation-at-separator))

            ((eq current-token 'end-tokens)
             (cond ((or (equal following-token separator)
                        (equal following-token stmt-separator))
                    (purs-indentation-add-indentation starter-indent)
                    (throw 'parse-end nil)))
             (throw 'return nil))

            (t (throw 'return nil))))))

(defun purs-indentation-at-separator ()
  (let ((separator-column
         (and (= (purs-current-column) (purs-indentation-current-indentation))
              (purs-current-column))))
    (purs-indentation-read-next-token)
    (cond ((eq current-token 'end-tokens)
           (purs-indentation-add-indentation current-indent)
           (throw 'return nil))
          (separator-column ;; on the beginning of the line
           (setq current-indent (purs-current-column))
           (setq starter-indent separator-column)))))

(defun purs-indentation-implicit-layout-list (parser)
  (let* ((layout-indent (purs-current-column))
         (current-indent (purs-current-column))
         (left-indent (purs-current-column)))
    (catch 'return
      (while t
        (let ((left-indent left-indent))
          (funcall parser))
        (cond ((member current-token '(layout-next ";"))
               (purs-indentation-read-next-token))
              ((eq current-token 'end-tokens)
               (when (or (purs-indentation-expression-token following-token)
                         (string= following-token ";"))
                 (purs-indentation-add-layout-indent))
               (throw 'return nil))
              (t (throw 'return nil))))))
  ;; put purs-indentation-read-next-token outside the current-indent definition
  ;; so it will not return 'layout-end again
  (when (eq current-token 'layout-end)
    (purs-indentation-read-next-token))) ;; leave layout at 'layout-end or illegal token

(defun purs-indentation-phrase (phrase)
  (purs-indentation-with-starter
   `(lambda () (purs-indentation-phrase-rest ',phrase))
   nil))

(defun purs-indentation-phrase-rest (phrase)
  (let ((starter-line parse-line-number))
    (let ((current-indent (purs-current-column)))
      (funcall (car phrase)))
    (cond
     ((eq current-token 'end-tokens)
      (cond ((null (cdr phrase))) ;; fallthrough
            ((equal following-token (cadr phrase))
             (purs-indentation-add-indentation starter-indent)
             (throw 'parse-end nil))
            ((string= (cadr phrase) "in")
             (when (= left-indent layout-indent)
               (purs-indentation-add-layout-indent)
               (throw 'parse-end nil)))
            (t (throw 'parse-end nil))))

     ((null (cdr phrase)))

     ((equal (cadr phrase) current-token)
      (let* ((on-new-line (= (purs-current-column) (purs-indentation-current-indentation)))
             (lines-between (- parse-line-number starter-line))
             (left-indent (if (<= lines-between 0)
                              left-indent
                            starter-indent)))
        (purs-indentation-read-next-token)
        (when (eq current-token 'end-tokens)
          (purs-indentation-add-indentation
           (cond ((member (cadr phrase) '("then" "else"))
                  (+ starter-indent purs-indentation-ifte-offset))
                 ((member (cadr phrase) '("in" "->"))
                  ;; expression ending in another expression
                  (if on-new-line
                      (+ left-indent purs-indentation-starter-offset)
                    left-indent))
                 (t (+ left-indent purs-indentation-left-offset))))
          (throw 'parse-end nil))
        (purs-indentation-phrase-rest (cddr phrase))))

     ((string= (cadr phrase) "in"))))) ;; fallthrough

(defun purs-indentation-add-indentation (indent)
  (purs-indentation-push-indentation
   (if (<= indent layout-indent)
       (+ layout-indent purs-indentation-layout-offset)
     indent)))

(defun purs-indentation-add-layout-indent ()
  (purs-indentation-push-indentation layout-indent))

(defun purs-indentation-add-where-pre-indent ()
  (purs-indentation-push-indentation
   (+ layout-indent purs-indentation-where-pre-offset))
  (if (= layout-indent purs-indentation-layout-offset)
      (purs-indentation-push-indentation
       purs-indentation-where-pre-offset)))

(defun purs-indentation-add-where-post-indent (indent)
  (purs-indentation-push-indentation
   (+ indent purs-indentation-where-post-offset)))

(defun purs-indentation-push-indentation (indent)
  (when (or (null possible-indentations)
            (< indent (car possible-indentations)))
    (setq possible-indentations
          (cons indent possible-indentations))))

(defun purs-indentation-token-test ()
  (let ((current-token nil)
        (following-token nil)
        (layout-indent 0)
        (parse-line-number 0)
        (indentation-point (mark)))
    (purs-indentation-read-next-token)))

(defun purs-indentation-read-next-token ()
  (cond ((eq current-token 'end-tokens)
         'end-tokens)
        ((eq current-token 'layout-end)
         (cond ((> layout-indent (purs-current-column))
                'layout-end)
               ((= layout-indent (purs-current-column))
                (setq current-token 'layout-next))
               ((< layout-indent (purs-current-column))
                (setq current-token (purs-indentation-peek-token)))))
        ((eq current-token 'layout-next)
         (setq current-token (purs-indentation-peek-token)))
        ((> layout-indent (purs-current-column))
         (setq current-token 'layout-end))
        (t
         (purs-indentation-skip-token)
         (if (>= (point) indentation-point)
             (progn
               (setq following-token
                     (if (= (point) indentation-point)
                         (purs-indentation-peek-token)
                       'no-following-token))
               (setq current-token 'end-tokens))
           (when (= (purs-current-column) (purs-indentation-current-indentation))
             ;; on a new line
             (setq current-indent (purs-current-column))
             (setq left-indent (purs-current-column))
             (setq parse-line-number (+ parse-line-number 1)))
           (cond ((> layout-indent (purs-current-column))
                  (setq current-token 'layout-end))
                 ((= layout-indent (purs-current-column))
                  (setq current-token 'layout-next))
                 (t (setq current-token (purs-indentation-peek-token))))))))

(defun purs-indentation-peek-token ()
  "Return token starting at point."
  (cond ((looking-at "\\(if\\|then\\|else\\|let\\|in\\|ado\\|mdo\\|rec\\|\\(?:[[:word:]]+\\.\\)*do\\|proc\\|case\\|of\\|where\\|module\\|deriving\\|data\\|type\\|newtype\\|class\\|instance\\)\\([^[:alnum:]'_]\\|$\\)")
         (match-string-no-properties 1))
        ((looking-at "[][(){}[,;]")
         (match-string-no-properties 0))
        ((looking-at "\\(\\\\\\|->\\|<-\\|::\\|=\\||\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
         (match-string-no-properties 1))
        ((looking-at "\\(→\\|←\\|∷\\)\\([^-:!#$%&*+./<=>?@\\\\^|~]\\|$\\)")
         (let ((tok (match-string-no-properties 1)))
           (or (cdr (assoc tok purs-indentation-unicode-tokens)) tok)))
        ((looking-at"[-:!#$%&*+./<=>?@\\\\^|~`]" )
         'operator)
        (t 'value)))

(defun purs-indentation-skip-token ()
  "Skip to the next token."
  (let ((case-fold-search nil))

    (if (or (looking-at "'\\([^\\']\\|\\\\.\\)*'")
            (looking-at "\"\\([^\\\"]\\|\\\\.\\)*\"")
            (looking-at         ; Hierarchical names always start with uppercase
             "[[:upper:]]\\(\\sw\\|'\\)*\\(\\.\\(\\sw\\|'\\)+\\)*")
            (looking-at "\\sw\\(\\sw\\|'\\)*") ; Only unqualified vars can start with lowercase
            (looking-at "[0-9][0-9oOxXeE+-]*")
            (looking-at "[-:!#$%&*+./<=>?@\\\\^|~]+")
            (looking-at "[](){}[,;]")
            (looking-at "`[[:alnum:]']*`"))
        (goto-char (match-end 0))
      ;; otherwise skip until space found
      (skip-syntax-forward "^-"))
    (forward-comment (buffer-size))))

(provide 'purs-indentation)

;; Local Variables:
;; tab-width: 8
;; End:

;;; purs-indentation.el ends here
