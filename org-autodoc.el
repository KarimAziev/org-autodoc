;;; org-autodoc.el --- Generate overview for elisp packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/org-autodoc
;; Keywords: convenience, docs
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1") (org "9.5.5"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Generate overview and use-package skeletons for elisp packages.

;;; Commands

;;; Code:



(require 'subr-x)
(declare-function find-library-name "find-func")
(defvar org-autodoc-load-filename (or load-file-name buffer-file-name))

(defvar org-autodoc-docstring-positions
  (mapcar (lambda (it)
            (setcar it (intern (car it)))
            it)
          '(("defun" . 3)
            ("defmacro" . 3)
            ("defsubst" . 4)
            ("defcustom" . 3)
            ("define-skeleton" . 2)
            ("define-compilation-mode" . 3)
            ("define-minor-mode" . 2)
            ("define-derived-mode" . 4)
            ("define-generic-mode" . 8)
            ("ert-deftest" . 3)
            ("cl-defun" . 3)
            ("cl-defsubst" . 3)
            ("cl-defmacro" . 3)
            ("cl-defgeneric" . 3)
            ("cl-defmethod" . 6)
            ("defalias" . 4)
            ("defhydra" . 4)
            ("defgroup" . 3)
            ("deftheme" . 3)
            ("define-widget" . 3)
            ("transient-define-suffix" . 3)
            ("transient-define-argument" . 3)
            ("transient-define-prefix" . 3)
            ("defvar" . 4)
            ("defvar-local" . 4)
            ("cl-defstruct" . 3)
            ("easy-mmode-define-minor-mode" . 2)
            ("transient-define-infix" . 3)
            ("defface" . 3))))

(defvar org-autodoc-group-annotation-alist
  '((:define-derived-mode . "Major mode")
    (:define-generic-mode . "Major mode")
    (:define-compilation-mode . "Compilation mode")
    (:easy-mmode-define-minor-mode . "Minor mode")
    (:define-minor-mode . "Minor mode")
    (:transient-define-prefix . "Transient")
    (:define-generic-mode . "Generic mode")
    (:keymap . "Keymaps")
    (:defhydra . "Hydras")
    (:use-package . "Used Packages")
    (:interactive . "Commands")
    (:defcustom . "Customization")
    (:defun . "Functions")
    (:defalias . "Functions")
    (:cl-defun . "Functions")
    (:defmacro . "Macros")
    (:cl-defmacro . "Macros")
    (:defvar . "Variables")
    (:defvar-local . "Variables")
    (:cl-defmethod . "Method")
    (:cl-defstruct . "Structs")
    (:defsubst . "Inline Functions")
    (:cl-defsubst . "Inline Functions")))

(defmacro org-autodoc--rpartial (fn &rest args)
  "Return a partial application of FN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FN. The result is a new
function which does the same as FN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  `(lambda (&rest pre-args)
     ,(car (list (if (symbolp fn)
                     `(apply #',fn (append pre-args (list ,@args)))
                   `(apply ,fn (append pre-args (list ,@args))))))))

(defmacro org-autodoc-pipe (&rest functions)
  "Return left-to-right composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(lambda (&rest args)
     ,@(let ((init-fn (pop functions)))
         (list
          (seq-reduce
           (lambda (acc fn)
             (if (symbolp fn)
                 `(funcall #',fn ,acc)
               `(funcall ,fn ,acc)))
           functions
           (if (symbolp init-fn)
               `(apply #',init-fn args)
             `(apply ,init-fn args)))))))

(defmacro org-autodoc--compose (&rest functions)
  "Return right-to-left composition from FUNCTIONS."
  (declare (debug t) (pure t) (side-effect-free t))
  `(org-autodoc-pipe ,@(reverse functions)))

(defmacro org-autodoc-with-temp-lisp-buffer (&rest body)
  "Execute BODY in temp buffer with Emacs Lisp mode without hooks."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (erase-buffer)
     (let (emacs-lisp-mode-hook) (emacs-lisp-mode))
     (progn
       ,@body)))

(defmacro org-autodoc--up-list-until-nil (&rest body)
  "Move backward up across and execute BODY until it's return value is nil."
  `(save-excursion
    (let ((result))
      (while (and (null result)
                  (org-autodoc-backward-up-list))
        (setq result (progn ,@body)))
      result)))

(defun org-autodoc-bounds-of-src-block ()
  "Return list of (BLOCK-TYPE BEGINNING END).
Beginning and end is bounds of inner content. For example: (example 4292 4486)."
  (save-excursion
    (save-restriction
      (widen)
      (beginning-of-line)
      (skip-chars-forward "\s\t")
      (let ((case-fold-search t)
            (block-start-re
             "[,]?#\\+\\(begin\\)_\\([a-z]+\\)\\($\\|[\s\f\t\n\r\v]\\)")
            (prefix))
        (if (looking-at block-start-re)
            (let ((open-block (match-string-no-properties 0))
                  (block-type (match-string-no-properties 2))
                  (content-beg (progn (forward-line 1)
                                      (point))))
              (setq block-type (downcase block-type))
              (setq prefix (if (string= "," (substring open-block 0 1)) "," ""))
              (when (re-search-forward (concat
                                        prefix "#\\+\\(end\\)_" "\\("
                                        (regexp-quote
                                         (downcase block-type))
                                        "\\)" "\\($\\|[\s\f\t\n\r\v]\\)")
                                       nil t 1)
                (let ((content-end (match-beginning 0)))
                  (list block-type
                        content-beg
                        content-end))))
          (when
              (re-search-forward
               "[,]?#\\+\\(begin\\|end\\)_\\([a-z]+\\)\\($\\|[\s\f\t\n\r\v]\\)"
               nil t 1)
            (when-let ((word (match-string-no-properties 1))
                       (prefix (if (string= ","
                                            (substring-no-properties
                                             (match-string-no-properties 0)
                                             0 1))
                                   ","
                                 ""))
                       (structure-type (match-string-no-properties 2))
                       (end (match-beginning 0)))
              (when (string= (downcase word) "end")
                (when (re-search-backward (concat prefix
                                                  "#\\+\\(begin\\)_" "\\("
                                                  (regexp-quote
                                                   (downcase structure-type))
                                                  "\\)" "\\($\\|[\s\f\t\n\r\v]\\)")
                                          nil t 1)
                  (forward-line 1)
                  (list (downcase structure-type)
                        (point)
                        end))))))))))

(defun org-autodoc-measure-columns (rows &optional head)
  "Return list with longest columns sizes in ROWS and HEAD."
  (let ((columns-number (length (car (seq-sort-by #'length '> rows))))
        (items (if head (append (list head)
                                rows)
                 rows))
        (lengths))
    (dotimes (idx columns-number)
      (let ((longest
             (nth idx
                  (car
                   (seq-sort-by (lambda (it)
                                  (if-let ((col (nth idx it)))
                                      (length (format "%s" col))
                                    0))
                                #'> items)))))
        (setq lengths (nconc lengths (list (if longest
                                               (length (format "%s" longest))
                                             0))))))
    lengths))

(defun org-autodoc-render-list-to-org-table (rows &optional head)
  "Convert list or vector of ROWS to org table wit HEAD."
  (when (vectorp rows)
    (setq rows (append rows nil)))
  (setq rows (mapcar (lambda (r)
                       (cond ((vectorp r)
                              (append r nil))
                             ((not (listp (cdr r)))
                              (list (car r)
                                    (cdr r)))
                             (t r)))
                     rows))
  (let* ((columns (seq-map-indexed
                   (lambda (_val idx)
                     (or
                      (format "%s" (or (nth idx head)
                                       (1+ idx)))))
                   (car (seq-sort-by #'length '> rows))))
         (columns-sizes (org-autodoc-measure-columns rows head))
         (formatted-rows
          (mapcar (lambda (r)
                    (mapcar
                     (apply-partially #'format "%s") r))
                  rows))
         (rows-str (mapconcat
                    (lambda (row)
                      (concat
                       "|"
                       (string-join
                        (seq-map-indexed
                         (lambda (col idx)
                           (setq col (if col (format " %s " col)))
                           (let ((indent
                                  (- (+ 2 (nth idx columns-sizes))
                                     (length (or col "")))))
                             (concat col (make-string indent ?\ ))))
                         row)
                        "|")
                       "|"))
                    formatted-rows "\n"))
         (header (concat
                  "|"
                  (string-join
                   (seq-map-indexed
                    (lambda (col idx)
                      (setq col (if col (format " %s " col)))
                      (let ((indent
                             (- (+ 2
                                   (nth idx columns-sizes))
                                (length col))))
                        (if (> indent 0)
                            (concat col (make-string indent ?\ ))
                          col)))
                    columns)
                   "|")
                  "| " "\n" (concat "|"
                                    (string-join
                                     (seq-map-indexed
                                      (lambda (_col idx)
                                        (make-string
                                         (+ 2 (nth idx columns-sizes))
                                         ?-))
                                      columns)
                                     "+")
                                    "|"))))
    (string-join
     (list header rows-str)
     "\n")))

(defun org-autodoc-format-keymap-to-org (keymap)
  "Convert KEYMAP to org table."
  (when-let* ((map (and (keymapp keymap)
                        (org-autodoc-format-keymap-to-alist keymap)))
              (filtered-map
               (if-let* ((package-name (org-autodoc-get-provide))
                         (lib (symbol-name package-name)))
                   (seq-filter
                    (lambda (it)
                      (string-prefix-p lib
                                       (symbol-name
                                        (cdr
                                         it))) )
                    map)
                 map)))
    (org-autodoc-render-list-to-org-table
     filtered-map
     (list "Key" "Command"))))

(defun org-autodoc-doc-to-org (doc-str)
  "Transform DOC-STR to org text."
  (mapconcat
   (lambda (str)
     (let ((prefix (or (string-prefix-p "`" str)
                       (string-prefix-p "‘" str))))
       (cond ((and prefix
                   (string-suffix-p "'" str))
              (concat "~" (substring str 1 (1- (length str))) "~"))
             ((and prefix
                   (string-match-p "'[,.]$" str))
              (let ((len (length str)))
                (concat "~" (substring str 1 (1- (1- len)))
                        (substring str (1- len)
                                   len)
                        "~")))
             ((let ((case-fold-search nil))
                (and (string-match-p "[A-Z]" str)
                     (not (string-match-p "[A-Z][a-z]" str))))
              (concat "~" (downcase str) "~"))
             (t str))))
   (split-string doc-str nil t)
   "\s"))

(defun org-autodoc-annotate-to-org (item-list)
	"Format ITEM-LIST to org list item.
ITEM-LIST is a list of (NAME ARGS DOC-STRING DEFINITION-TYPE).
For example:
\(\"my-function\" (my-arg) \"Doc string.\" defun)"
  (let ((name (format "*** ~%s~" (car item-list)))
        (args (when (nth 1 item-list)
                (format " %s" (nth 1 item-list))))
        (doc (when (and (nth 2 item-list)
                        (stringp (nth 2 item-list)))
               (org-autodoc-doc-to-org (nth 2 item-list))))
        (title)
        (keymap (when (and (memq (car (last item-list))
                                 '(keymap)))
                  (when-let* ((sym (intern (car item-list)))
                              (val (and (symbolp sym)
                                        (boundp sym)
                                        (symbol-value sym))))
                    (org-autodoc-format-keymap-to-org val)))))
    (setq title (string-join (delete nil (list name args)) "\s"))
    (string-join (delete nil (list title (when doc (substitute-command-keys
                                                    doc))
                                   keymap))
                 "\n")))

(defun org-autodoc-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun org-autodoc-function-p (symb)
  "Return t is SYMB can have arguments.
SYMB can be either symbol, either string."
  (member (if (symbolp symb)
              (symbol-name symb)
            symb)
          '("defun"
            "defmacro"
            "defun*"
            "defsubst"
            "cl-defun"
            "define-inline"
            "cl-defgeneric"
            "cl-defmethod"
            "define-advice")))

(defun org-autodoc-symbol-keymapp (sym)
  "Return t if value of symbol SYM is a keymap."
  (when-let ((val (when (boundp sym) (symbol-value sym))))
    (keymapp val)))

(defun org-autodoc-symbol-sexp-keymapp (sexp)
  "Return t if SEXP look like keymap variable."
  (when-let* ((value (nth 2 sexp))
              (vals (and (listp value)
                         (symbolp (car value))
                         (memq (car value) '(let let*))
                         (car (seq-drop value 1)))))
    (when (and (listp vals)
               (listp (car vals)))
      (seq-find (lambda (it)
                (when-let ((val (and (listp (cdr it))
                                     (listp (cadr it))
                                     (cadr it))))
                  (and
                   (= 1 (length val))
                   (symbolp (car val))
                   (memq (car val) '(make-sparse-keymap)))))
              vals))))

(defun org-autodoc-backward-list (&optional n)
  "Move backward across N balanced group of parentheses.
Return new position if changed, nil otherwise."
  (let ((pos (point))
        (end))
    (setq end (ignore-errors
                (backward-list (or n 1))
                (point)))
    (unless (equal pos end)
      end)))

(defun org-autodoc-get-provide ()
  "Parse list at point and return alist of form (symbol-name args doc deftype).
E.g. (\"org-autodoc-parse-list-at-point\" (arg) \"Doc string\" defun)"
  (save-excursion
    (goto-char (point-max))
    (let ((found))
      (while (and
              (not found)
              (org-autodoc-backward-list))
        (let ((l (list-at-point)))
          (when (and (listp l)
                     (symbolp (car l))
                     (eq 'provide (car l))
                     (symbolp (org-autodoc-unquote (nth 1 l))))
            (setq found (org-autodoc-unquote (nth 1 l))))))
      found)))

(defun org-autodoc-parse-list-at-point ()
  "Parse list at point and return alist of form (symbol-name args doc deftype).
E.g. (\"org-autodoc-parse-list-at-point\" (arg) \"Doc string\" defun)"
  (when-let* ((sexp (unless (nth 4 (syntax-ppss (point)))
                      (list-at-point)))
              (type (car sexp))
              (id (org-autodoc-unquote (when (symbolp (nth 1 sexp))
                                         (nth 1 sexp))))
              (name (symbol-name id)))
    (let ((doc (when-let ((pos (cdr (assq type
                                          org-autodoc-docstring-positions))))
                 (nth pos sexp)))
          (args (and (org-autodoc-function-p type)
                     (nth 2 sexp))))
      (list name args doc
            (cond ((and (org-autodoc-function-p type)
                        (or (and
                             (nth 3 sexp)
                             (listp (nth 3 sexp))
                             (symbolp (car (nth 3 sexp)))
                             (eq 'interactive (car (nth 3 sexp))))
                            (and
                             (nth 4 sexp)
                             (listp (nth 4 sexp))
                             (symbolp (car (nth 4 sexp)))
                             (eq 'interactive (car (nth 4 sexp))))))
                   'interactive)
                  ((or (org-autodoc-symbol-keymapp id)
                       (org-autodoc-symbol-sexp-keymapp sexp))
                   'keymap)
                  (t type))))))

(defun org-autodoc-format-sexp-to-require (sexp)
  "Return string with package name if SEXP is valid require call.
If package is optional, also add suffix (optional)."
  (pcase sexp
    (`(require ,(and name
                     (guard (listp name))
                     (guard (eq (car-safe name) 'quote))))
     (cons (org-autodoc-unquote name) nil))
    (`(require ,(and name
                     (guard (listp name))
                     (guard (eq (car-safe name) 'quote)))
               ,_)
     (cons (org-autodoc-unquote name) nil))
    (`(require ,(and name
                     (guard (listp name))
                     (guard (eq (car-safe name) 'quote)))
               ,_
               ,(and optional (guard (not (eq optional nil)))))
     (cons (org-autodoc-unquote name) t))))

(defun org-autodoc-parse-require ()
  "Parse list at point and return alist of form (symbol-name args doc deftype).
E.g. (\"org-autodoc-parse-list-at-point\" (arg) \"Doc string\" defun)"
  (when-let ((sexp (unless (or (nth 4 (syntax-ppss (point)))
                               (nth 3 (syntax-ppss (point))))
                     (sexp-at-point))))
    (when (listp sexp)
      (org-autodoc-format-sexp-to-require sexp))))

(defun org-autodoc-get-require ()
  "Return string in `org-mode' format with package dependencies."
  (let ((requires '())
        (deps))
    (save-excursion
      (goto-char (point-max))
      (while (org-autodoc-backward-list)
        (when-let ((sexp (unless (nth 4 (syntax-ppss (point)))
                           (list-at-point))))
          (if-let ((dep (org-autodoc-format-sexp-to-require sexp)))
              (push dep requires)
            (when (listp sexp)
              (push sexp deps))))))
    (when deps
      (org-autodoc-with-temp-lisp-buffer
          (insert (prin1-to-string deps))
          (while (re-search-backward "[(]require[\s\t\n\r\f]+'" nil t 1)
            (when-let ((found (org-autodoc-parse-require)))
              (unless (member found requires)
                (push found requires))))))
    requires))

(defun org-autodoc-generate-requirenments ()
  "Return string in `org-mode' format with package dependencies."
  (let ((package-requires (org-autodoc-get-require-from-package-header))
        (requires (org-autodoc-get-require))
        (result))
    (dolist (it package-requires)
      (let ((name (if (listp it) (car it) it))
            (version (when (listp it)
                       (cadr it)))
            (found)
            (str))
        (setq found (seq-find (lambda (c) (eq (car c) name))
                              requires))
        (setq str (string-join
                   (delq nil
                         (list
                          (pcase name
                            ('emacs (format "Emacs >= %s" (cadr it)))
                            ((pred symbolp)
                             (string-join (delete
                                           nil
                                           (list (symbol-name name)
                                                 version))
                                          " "))
                            ((pred stringp)
                             (string-join (delete nil
                                                  (list name version))
                                          " ")))
                          (when found "(optional)")))
                   "\s"))
        (push str result)))
    (nconc result
           (mapcar
            (lambda (it)
              (concat (symbol-name (car it))
                      (when (cdr it)
                        " (optional)")))
            requires))))

(defun org-autodoc-format-keymap-to-alist (keymap)
  "Convert KEYMAP to alist."
  (when (keymapp keymap)
    (with-temp-buffer
      (describe-map-tree keymap t nil nil nil t t t)
      (while (re-search-backward "[\n][\n]+" nil t 1)
        (replace-match "\n"))
      (let* ((items (seq-remove
                     (apply-partially #'string-match-p "^[<]menu-bar[>]")
                     (split-string
                      (string-trim (buffer-substring-no-properties
                                    (point-min)
                                    (point-max)))
                      "[\n\r\f]"
                      t)))
             (rows (delete nil (mapcar (lambda (it)
                                         (let* ((parts (reverse
                                                        (split-string it
                                                                      "[\s\t]"
                                                                      t)))
                                                (cmd (pop parts))
                                                (key (string-join
                                                      (reverse parts) "\s")))
                                           (when (and
                                                  (key-valid-p key)
                                                  (if (string-match-p "^<" key)
                                                      (string-match-p
                                                       "<remap> "
                                                       key)
                                                    t)
                                                  (not (member
                                                        cmd
                                                        '("[lambda]"
                                                          "[closure]"
                                                          "[byte-code]"))))
                                             (cons
                                              key
                                              (intern cmd)))))
                                       items))))
        rows))))

(defun org-autodoc-git-remotes-alist ()
  "Return alist of remotes and associated urls (REMOTE-NAME . REMOTE-URL)."
  (when-let ((remotes
              (with-temp-buffer
                (when (= 0 (apply #'call-process "git" nil t nil
                                  '("remote" "-v")))
                  (string-trim (buffer-string))))))
    (seq-uniq
     (mapcar (lambda (l) (let ((parts (split-string l)))
                      (cons (car parts)
                            (cadr parts))))
             (split-string remotes "\n" t)))))

(defun org-autodoc-scan-get-buffer-maps ()
  "Return keymaps in current buffer."
  (when-let ((maps (plist-get (org-autodoc-scan-buffer) :keymap)))
    (mapcar (lambda (it)
              (let ((sym (intern (car it))))
                (cons sym (symbol-value sym))))
            maps)))

(defun org-autodoc-scan-buffer ()
  "Return plist of top level Lisp definitions.

Each key is definition type, converted to keyword (:defmacro, :defun etc),
except interactive functions, which holds under keyword :interactive.

The value of plist is a list of sublists of form (symbol-name args doc deftype).

See function `org-autodoc-parse-list-at-point'."
  (save-excursion
    (let ((pl '()))
      (goto-char (point-max))
      (while (org-autodoc-backward-list)
        (when-let ((sexp (org-autodoc-parse-list-at-point)))
          (let ((keyword (intern (concat ":" (symbol-name (car
                                                           (reverse sexp)))))))
            (if-let ((group (plist-get pl keyword)))
                (setq pl (plist-put pl keyword (append group (list sexp))))
              (setq pl (plist-put pl keyword (list sexp)))))))
      pl)))

(defun org-autodoc-elisp-generate-use-package-string (reponame
                                                      &optional
                                                      user
                                                      commands
                                                      maps)
  "Generate string straght and use package installation from USER and REPONAME.
With COMMANDS also insert :commands.
With MAPS also insert :bind."
  (org-autodoc-with-temp-lisp-buffer
      (insert
       "(use-package " reponame ")")
      (forward-char -1)
    (when (and user reponame)
      (newline-and-indent)
      (insert ":straight (" reponame ")")
      (forward-char -1)
      (newline-and-indent)
      (insert (format ":repo \"%s/%s\"" user reponame))
      (newline-and-indent)
      (insert ":type git")
      (newline-and-indent)
      (insert ":host github")
      (forward-char 1))
    (let ((key-commands))
      (when maps
        (newline-and-indent)
        (insert ":bind ()")
        (forward-char -1)
        (dotimes (i (length maps))
          (let ((it (nth i maps)))
            (let ((name (symbol-name (car it)))
                  (alist (org-autodoc-format-keymap-to-alist (cdr it)))
                  (el))
              (when (> i 0)
                (newline-and-indent))
              (insert "(:map " name ")")
              (forward-char -1)
              (while (setq el (pop alist))
                (newline-and-indent)
                (insert  (if (string-match-p "<remap>" (car el))
                             (concat "([remap " (car (last (split-string
                                                            (car el)
                                                            "[\"\s\t<>]"
                                                            t)))
                                     "] . "
                                     (prin1-to-string (cdr el))")")
                           (prin1-to-string el)))
                (push (symbol-name (cdr el)) key-commands))
              (forward-char 1))))
        (forward-char 1))
      (setq commands (seq-difference commands key-commands))
      (when commands
        (newline-and-indent)
        (insert ":commands ()")
        (forward-char -1)
        (dotimes (i (length commands))
          (let ((cell (nth i commands)))
            (when (> i 0)
              (newline-and-indent))
            (insert cell)))
        (forward-char 1))
      (forward-sexp -1)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-autodoc-annotate-with (prefix fn)
  "Return string of grouped annotations.

Each group is prefixed with PREFIX, and constists of
results of calling FN with list of (symbol-name args doc deftype)."
  (when-let ((items (org-autodoc-scan-buffer)))
    (let ((blocks))
      (dolist (key (mapcar #'car org-autodoc-group-annotation-alist))
        (when-let ((title (alist-get key org-autodoc-group-annotation-alist))
                   (description (plist-get items key)))
          (setq description
                (concat prefix
                        title
                        "\n\n"
                        (funcall fn description)
                        "\n"))
          (push description blocks)))
      (when blocks
        (string-join (reverse blocks))))))

(defun org-autodoc-get-emacs-batch-cmd (&optional fn file)
  "Return string for running `emacs' evaluating FN in FILE."
  (let ((fn (or fn "buffer-file-name")))
    (string-join `("emacs --batch " ,@(mapcan (lambda (path) (list "-L" path))
                                              (append (list "./")
                                                      load-path))
                   ,(concat " --file " (or file buffer-file-name))
                   "-l "
                   ,org-autodoc-load-filename
                   ,(if file
                        (concat "-l " file)
                      "")
                   "\s"
                   " --eval "
                   ,(prin1-to-string (if fn
                                         (format
                                          "(progn %s)" fn)
                                       (format "(print (eval-buffer))"))))
                 "\s")))

(defun org-autodoc-annotate-as-org-list (sexps)
  "Return string with generetad from SEXPS annotations as org list items."
  (mapconcat
   (lambda (s) (org-autodoc-annotate-to-org s))
   sexps "\n"))

(defun org-autodoc-get-require-from-package-header ()
  "Return list of packages defined in Package-Requires header."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^;;[\s]Package-Requires:\\([^\n]+\\)?" nil t 1)
      (car (read-from-string (match-string-no-properties 1))))))

;;;###autoload
(defun org-autodoc-insert-use-package (&optional library)
  "Insert `use-package' skeleton for LIBRARY."
  (interactive)
  (require 'find-func)
  (let ((orig-buffer (current-buffer)))
    (when-let* ((lib (or library (read-library-name)))
                (file (find-library-name lib))
                (str (with-current-buffer (find-file-noselect file)
                       (require (intern lib))
                       (org-autodoc-elisp-generate-use-package-string
                        lib nil
                        (mapcar #'car (plist-get (org-autodoc-scan-buffer)
                                                 :interactive))
                        (org-autodoc-scan-get-buffer-maps)))))
      (with-current-buffer orig-buffer
        (insert str)))))



;;;###autoload
(defun org-autodoc-org-annotation ()
  "Return string with readme template in org mode format.
If OUTPUT-FILE is non nil, write template to OUTPUT-FILE."
  (when-let* ((remote (cdr (car (org-autodoc-git-remotes-alist))))
              (parts (reverse
                      (seq-take (reverse
                                 (split-string
                                  (replace-regexp-in-string
                                   "\\.git$"
                                   ""
                                   (cdr (car (org-autodoc-git-remotes-alist))))
                                  "[:/]"
                                  t))
                                2)))
              (user (pop parts))
              (name (pop parts)))
    (let ((requirements (org-autodoc-generate-requirenments))
          (synopsis (progn
                      (require 'lisp-mnt nil t)
                      (when-let ((descr
                                  (when (fboundp 'lm-synopsis)
                                    (lm-synopsis))))
                        (setq descr (string-trim descr))
                        (capitalize (if (and
                                         (> (length descr) 0)
                                         (not (string-match-p "\\.$" descr)))
                                        (concat descr ".")
                                      descr)))))
          (str))
      (setq str (string-join
                 (delq nil
                       `(,(format "* %s" name)
                         ,synopsis
                         ,(when requirements
                            (concat "** Requirements\n\n"
                                    (mapconcat (apply-partially #'format "+ %s")
                                               requirements "\n")))
                         "** Installation"
                         "*** Manual"
                         "Download the source code and put it wherever you like and add the directory to the load path:"
                         "#+begin_src elisp :eval no"
                         ,(format "(add-to-list 'load-path \"/path/to/%s)"
                                  name)
                         ,(format "(require '%s)" name)
                         "#+end_src"
                         "*** With use-package and straight"
                         "#+begin_src elisp :eval no"
                         ,(org-autodoc-elisp-generate-use-package-string
                           name user
                           (mapcar #'car (plist-get (org-autodoc-scan-buffer)
                                                    :interactive))
                           (org-autodoc-scan-get-buffer-maps))
                         "#+end_src"
                         ,(org-autodoc-annotate-with
                           "** "
                           'org-autodoc-annotate-as-org-list)))
                 "\n\n"))
      (set-text-properties 0 (length str) nil str)
      str)))

;;;###autoload
(defun org-autodoc-async ()
  "Load current file in new `emacs' process and generate documentation."
  (interactive)
  (let* ((command
          (org-autodoc-get-emacs-batch-cmd
           "(message \"%s\" (org-autodoc-org-annotation))"
           buffer-file-name)))
    (let ((proc)
          (buffer (generate-new-buffer "*org-autodoc-annotation*")))
      (progn
        (with-current-buffer buffer
          (setq proc (start-process-shell-command
                      (nth 0
                           (split-string command))
                      buffer command))
          (shell-command-save-pos-or-erase)
          (when (fboundp 'shell-mode)
            (shell-mode))
          (view-mode +1))
        (set-process-sentinel
         proc
         (lambda (process _state)
           (let ((output (with-current-buffer
                             (process-buffer process)
                           (buffer-string))))
             (if (= (process-exit-status process) 0)
                 (progn
                   (when-let ((buff (process-buffer process)))
                     (when (and (bufferp buff)
                                (buffer-live-p buff))
                       (kill-buffer buff)))
                   (with-current-buffer (get-buffer-create "*org-autodoc*")
                     (let ((inhibit-read-only t))
                       (erase-buffer)
                       (insert
                        (replace-regexp-in-string "‘" "`"
                                                  (replace-regexp-in-string
                                                   "’" "'" output)))
                       (goto-char (point-max))
                       (let ((emacs-lisp-mode-hook))
                         (emacs-lisp-mode)
                         (while (re-search-backward
                                 "#[\\+]begin_src" nil t 1)
                           (when-let* ((bounds
                                        (org-autodoc-bounds-of-src-block))
                                       (str (buffer-substring-no-properties
                                             (nth 1 bounds)
                                             (nth 2 bounds)))
                                       (rep (org-autodoc-with-temp-lisp-buffer
                                                (insert str)
                                                (skip-chars-backward
                                                 "\s\t\n\r\f")
                                              (forward-sexp -1)
                                              (indent-sexp)
                                              (buffer-substring-no-properties
                                               (point-min)
                                               (point-max)))))
                             (replace-region-contents (nth 1 bounds)
                                                      (nth 2 bounds)
                                                      (lambda () rep)))))
                       (require 'org)
                       (org-mode)
                       (pop-to-buffer (current-buffer)))))
               (pop-to-buffer (process-buffer process))))))
        (require 'comint)
        (when (fboundp 'comint-output-filter)
          (set-process-filter proc #'comint-output-filter))))))

(defvar-local org-autodoc-current-doc-suffix nil)

(defvar-local org-autodoc-current-doc-prefix nil)

(defvar org-autodoc-symbols-to-narrow (mapcar #'car
                                              org-autodoc-docstring-positions))

(defun org-autodoc-re-search-backward-inner (regexp &optional bound count)
  "This function is helper for `km-elisp-re-search-backward'.
Search backward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-backward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (or (nth 3 parse))
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              (t
               (setq count (1- count)))))))
  (point))

(defun org-autodoc-re-search-forward-inner (regexp &optional bound count)
  "This function is helper for `org-autodoc-re-search-forward'.
Search forward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              (t
               (setq count (1- count)))))))
  (point))

(defun org-autodoc-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (unless n (setq n 1))
  (when-let ((str-start (nth 8 (syntax-ppss (point)))))
    (goto-char str-start))
  (let ((init-pos (point))
        (pos)
        (count n))
    (while (and (not (= count 0))
                (when-let ((end (ignore-errors
                                  (funcall fn)
                                  (point))))
                  (unless (= end (or pos init-pos))
                    (setq pos end))))
      (setq count (1- count)))
    (if (= count 0)
        pos
      (goto-char init-pos)
      nil)))

(defun org-autodoc-group-with (fn items &optional transform-fn)
  "Group ITEMS by calling FN with every item.
FN should return key.
TRANSFORM-FN should return transformed item."
  (seq-reduce (lambda (acc it)
                (let* ((key (funcall fn it))
                       (val (if transform-fn (funcall transform-fn it) it))
                       (cell (assoc key acc))
                       (group (if cell
                                  (append (cdr cell)
                                          (list val))
                                (list val))))
                  (if cell
                      (setcdr cell group)
                    (push (cons key group) acc))
                  acc))
              (seq-copy items) '()))

(defun org-autodoc-backward-up-list (&optional arg)
  "Move backward up across ARG balanced group of parentheses.
Return new position if changed, nil otherwise."
  (org-autodoc-move-with 'backward-up-list arg))

(defun org-autodoc-shared-start (s1 s2)
  "Return the longest prefix S1 and S2 have in common."
  (declare (pure t) (side-effect-free t))
  (let ((search-length (min (length s1) (length s2)))
        (i 0))
    (while (and (< i search-length)
                (= (aref s1 i) (aref s2 i)))
      (setq i (1+ i)))
    (substring s1 0 i)))

(defun org-autodoc-re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'org-autodoc-re-search-backward-inner)
               ((> count 0) #'org-autodoc-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err) (cdr err)))))))

(defun org-autodoc-shared-name-start (s1 s2)
  "Return the longest prefix S1 and S2 splitted."
  (let ((s1-names (split-string s1 "[-]"))
        (s2-names (split-string s2 "[-]"))
        (result))
    (while (when-let ((res1 (pop s1-names))
                      (res2 (pop s2-names)))
             (when (string= res1 res2)
               (setq result (push res1 result)))))
    (when result (string-join (reverse result) "-"))))

(defun org-autodoc-group-by-prefixes (strings)
  "Group STRINGS by longest common prefixes."
  (org-autodoc-group-with
   (lambda (str)
     (or (car
          (seq-sort-by
           #'length
           '>
           (delq nil
                 (mapcar
                  (apply-partially
                   #'org-autodoc-shared-name-start
                   str)
                  (remove str strings)))))
         ""))
   strings))


(defun org-autodoc-elisp-get-defun-names ()
  "Return names of functions defined with in current buffer."
  (let ((founds))
    (save-excursion
      (goto-char (point-min))
      (while (org-autodoc-re-search-forward
              (concat "[(]\\(\\(\\(cl-\\)?" (string-join
                                             (mapcar (org-autodoc--compose
                                                      symbol-name
                                                      car)
                                                     org-autodoc-docstring-positions)
                                             "\\|")
                      "\\)[\s\t\n]+\\([a-z][^\s\t\n;]+\\)\\)")
              nil t 1)
        (let ((name (match-string-no-properties 4)))
          (save-excursion
            (org-autodoc-backward-up-list)
            (unless (or (looking-back "['`]" 0)
                        (string-match-p "[)(]" name))
              (push name founds))))))
    founds))

(defun org-autodoc-count-matches-by-re (re str &optional start end)
  "Count occurrences of RE in STR.
START, inclusive, and END, exclusive, delimit the part of s to
match.  START and END are both indexed starting at 1; the initial
character in s is index 1."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (count-matches re (or start 1) (or end (point-max))))))

(defun org-autodoc--elisp-parse-list-at-point ()
  "Parse list at point and return alist of form (symbol-name args doc deftype).
E.g. (\"org-autodoc-parse-list-at-point\" (arg) \"Doc string\" defun)"
  (when-let* ((sexp (unless (nth 4 (syntax-ppss (point)))
                      (when-let ((s (sexp-at-point)))
                        (when (listp s)
                          s))))
              (type (car sexp))
              (id (org-autodoc-unquote
                   (when (symbolp (nth 1 sexp))
                     (nth 1 sexp))))
              (name (symbol-name id)))
    (let ((doc
           (when-let ((pos (cdr (assq type org-autodoc-docstring-positions))))
             (when (and (nth pos sexp)
                        (stringp (nth pos sexp)))
               (nth pos sexp))))
          (args (and (org-autodoc-function-p type)
                     (nth 2 sexp)))
          (interactivep (and (org-autodoc-function-p type)
                             (or (and
                                  (nth 3 sexp)
                                  (listp (nth 3 sexp))
                                  (symbolp (car (nth 3 sexp)))
                                  (eq 'interactive (car (nth 3 sexp))))
                                 (and
                                  (nth 4 sexp)
                                  (listp (nth 4 sexp))
                                  (symbolp (car (nth 4 sexp)))
                                  (eq 'interactive (car (nth 4 sexp)))))))
          (keymap (or (org-autodoc-symbol-keymapp id)
                      (org-autodoc-symbol-sexp-keymapp sexp)))
          (autoloaded (save-excursion
                        (forward-line -1)
                        (looking-at ";;;###autoload[^a-z]"))))
      (list
       :name name
       :args args
       :doc doc
       :type type
       :id id
       :keymap keymap
       :autoloaded autoloaded
       :interactive interactivep))))

(defun org-autodoc-elisp-bounds-of-def-sexp (&optional symbols)
  "Return bounds of first parent sexp which head is a member of SYMBOLS.
If SYMBOLS is nil use `km-elisp-function-symbols'"
  (org-autodoc--up-list-until-nil
   (when-let ((sexp (sexp-at-point)))
     (when-let ((start (and (listp sexp)
                            (nth 1 sexp)
                            (memq (car sexp)
                                  (or (if (listp symbols)
                                          symbols
                                        (and symbols (list symbols)))
                                      org-autodoc-symbols-to-narrow))
                            (point))))
       (forward-sexp 1)
       (cons start (point))))))

(defun org-autodoc-cycle-elisp-doc-detect-prefixes ()
  "Return longest prefix for functions in current buffer."
  (org-autodoc-group-by-prefixes (org-autodoc-elisp-get-defun-names)))

(defun org-autodoc-cycle-generate-function-docs (prefix function-name arguments
                                                        &optional ending)
  "Generate a verb based on FUNCTION-NAME with trimmed PREFIX and ARGUMENTS."
  (let ((replacements '(("&optional" . "Optional argument %s is\s.")
                        ("&rest" . "")
                        ("&key" . "")
                        ("buffer" . "IN BUFFER")
                        ("file" . "in FILE")
                        ("pos" . "at POS")))
        (verb (if
                  prefix
                  (string-trim
                   (replace-regexp-in-string "-" "\s"
                                             (replace-regexp-in-string
                                              (concat "^"  prefix)
                                              ""
                                              function-name)))
                function-name))
        (args (if (stringp arguments)
                  (split-string
                   (replace-regexp-in-string "[)(]" "" arguments) nil t)
                (mapcar #'symbol-name arguments)))
        (curr)
        (formatted-args)
        (parts)
        (first-word))
    (setq parts (delete nil (append (split-string verb)
                                    (list ending))))
    (setq first-word (capitalize (or (pop parts) "")))
    (setq verb (concat first-word
                       (if parts (concat " " (string-join parts "\s"))
                         "")))
    (while (setq curr (pop args))
      (let ((rep (cdr (assoc curr replacements)))
            (result))
        (setq result
              (if (null rep)
                  (format "%s is ." (upcase curr))
                (if-let ((count (org-autodoc-count-matches-by-re "%s" rep)))
                    (let ((next-args (mapcar #'upcase (seq-take args count))))
                      (setq args (seq-drop args count))
                      (apply #'format (append (list rep) next-args)))
                  rep)))
        (unless (string-empty-p result)
          (push result formatted-args))))
    (setq formatted-args (string-join (reverse formatted-args) "\n"))
    (string-trim-right (concat verb "." "\n" (or formatted-args "")))))

(defun org-autodoc-get-longest-prefixes ()
  "Return common prefix in current buffer."
  (mapcar #'car
          (seq-sort-by
           (org-autodoc--compose
            length
            car)
           #'>
           (delete nil
                   (org-autodoc-cycle-elisp-doc-detect-prefixes)))))



;;;###autoload
(defun org-autodoc-insert-doc-string ()
  "Insert documentation template for current definition."
  (interactive)
  (let* ((item
          (when-let ((l (org-autodoc--elisp-parse-list-at-point)))
            (when (assoc (plist-get l :type) org-autodoc-docstring-positions)
              l)))
         (bounds (if item (bounds-of-thing-at-point 'sexp)
                   (org-autodoc-elisp-bounds-of-def-sexp
                    (mapcar #'car
                            org-autodoc-docstring-positions)))))
    (unless item
      (when bounds
        (save-excursion
          (goto-char (car bounds))
          (setq item (org-autodoc--elisp-parse-list-at-point)))))
    (when (and item
               (not (plist-get item :doc)))
      (let ((name (plist-get item :name))
            (args (plist-get item :args))
            (prefix)
            (doc))
        (setq prefix
              (or (when name
                    (seq-find
                     (org-autodoc--rpartial string-prefix-p name)
                     (org-autodoc-get-longest-prefixes)))
                  ""))
        (setq org-autodoc-current-doc-prefix prefix)
        (setq doc (org-autodoc-cycle-generate-function-docs
                   org-autodoc-current-doc-prefix
                   name
                   args
                   org-autodoc-current-doc-suffix))
        (goto-char (car bounds))
        (down-list 1)
        (forward-sexp (cdr (assoc (plist-get item :type)
                                  org-autodoc-docstring-positions)))
        (newline-and-indent)
        (insert (prin1-to-string doc))
        (forward-sexp -1)
        (forward-char 1)))))

(provide 'org-autodoc)
;;; org-autodoc.el ends here