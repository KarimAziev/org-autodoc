;;; org-autodoc.el --- Generate overview for elisp packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/org-autodoc
;; Keywords: convenience, docs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;; Generate overview for elisp packages.

;;; Code:

(require 'shell)

(defvar org-autodoc-load-filename (or load-file-name buffer-file-name))
(defvar org-autodoc-group-annotation-alist
  '((:define-derived-mode . "Major mode")
    (:define-generic-mode . "Major mode")
    (:define-compilation-mode . "Compilation mode")
    (:easy-mmode-define-minor-mode . "Minor mode")
    (:define-minor-mode . "Minor mode")
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

(defvar org-autodoc-docstring-positions
  '((defcustom . 3)
    (defvar . 3)
    (defvar-local . 3)
    (defun . 3)
    (defmacro . 3)
    (defsubst . 3)
    (define-derived-mode . 4)
    (define-generic-mode . 7)
    (ert-deftest . 3)
    (cl-defun . 3)
    (cl-defsubst . 3)
    (cl-defmacro . 3)
    (cl-defmethod . 5)
    (defhydra . 3)
    (cl-defstruct . 2)
    (define-derived-mode . 4)
    (define-compilation-mode . 3)
    (easy-mmode-define-minor-mode . 2)
    (define-minor-mode . 2)
    (define-generic-mode . 7)))

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

(defmacro org-autodoc-with-temp-lisp-buffer (&rest body)
  "Execute BODY in temp buffer with Emacs Lisp mode without hooks."
  (declare (indent 2) (debug t))
  `(with-temp-buffer
     (erase-buffer)
     (let (emacs-lisp-mode-hook) (emacs-lisp-mode))
     (progn
       ,@body)))

(defun org-autodoc-format-keymap-to-alist (keymap)
	"Convert KEYMAP to alist."
  (when (keymapp keymap)
    (with-temp-buffer
      (describe-map-tree keymap)
      (while (re-search-backward "[\n][\n]+" nil t 1)
        (replace-match "\n"))
      (let* ((items (seq-remove
                     (apply-partially #'string-match-p "^[<]menu-bar[>]")
                     (split-string
                      (string-trim (buffer-substring-no-properties
                                    (point-min) (point-max)))
                      "[\n\r\f]"
                      t)))
             (rows (delete nil (mapcar (lambda (it)
                                         (let* ((parts (reverse (split-string it "[\s\t]" t)))
                                                (cmd (pop parts))
                                                (key (string-join (reverse parts) "\s")))
                                           (when (and (key-valid-p key)
                                                      (not (member cmd '("[lambda]" "[byte-code]"))))
                                             (cons
                                              key
                                              (intern cmd)))))
                                       items))))
        rows))))

(defun org-autodoc-git-remotes-alist ()
  "Return alist of remotes and associated urls (REMOTE-NAME . REMOTE-URL)."
  (when-let ((remotes
              (with-temp-buffer
                (when (= 0 (apply #'call-process "git" nil t nil '("remote" "-v")))
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

(defun org-autodoc-elisp-generate-use-package-string (user reponame &optional
                                                       commands
                                                       maps)
  "Generate string straght and use package installation from USER and REPONAME.
With COMMANDS also insert :commands.
With MAPS also insert :bind."
  (org-autodoc-with-temp-lisp-buffer
      (insert
       "(use-package " reponame ")")
      (forward-char -1)
    (newline-and-indent)
    (insert ":straight (" reponame ")")
    (forward-char -1)
    (newline-and-indent)
    (insert (format ":repo \"%s/%s\"" user reponame))
    (newline-and-indent)
    (insert ":type git")
    (newline-and-indent)
    (insert ":host github")
    (forward-char 1)
    (when maps
      (let ((names (mapcar (lambda (it) (symbol-name (car it)))
                           maps)))
        (setq commands (delete nil
                               (mapcar
                                (lambda (it) (unless (member (car it) names)
                                          it))
                                commands))))
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
              (insert (prin1-to-string el)))
            (forward-char 1))))
      (forward-char 1))
    (when commands
      (newline-and-indent)
      (insert ":commands ()")
      (forward-char -1)
      (dotimes (i (length commands))
        (let ((cell (nth i commands)))
          (when (> i 0)
            (newline-and-indent))
          (insert (car cell))))
      (forward-char 1))
    (forward-sexp -1)
    (buffer-string)))

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
                   "\s"
                   " --eval "
                   ,(prin1-to-string (if fn
                                         (format
                                          "(progn (eval-buffer) %s)" fn)
                                       (format "(print (eval-buffer))"))))
                 "\s")))

;;;###autoload
(defun org-autodoc-org-annotation (&optional output-file)
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
              (name (pop parts))
              (maps (org-autodoc-scan-get-buffer-maps)))
    (let ((items (org-autodoc-scan-buffer))
          (str))
      (setq str (string-join
                 (list
                  (format "* %s" name)
                  "** Installation"
                  "*** Manually"
                  "Download repository and it to your load path in your init file:"
                  "#+begin_src elisp :eval no"
                  (format "(add-to-list 'load-path \"/path/to/%s/\")" name)
                  (format "(require '%s)" name)
                  "#+end_src"
                  "*** With use-package and straight"
                  "#+begin_src elisp :eval no"
                  (org-autodoc-elisp-generate-use-package-string
                   user name
                   (plist-get items :interactive)
                   maps)
                  "#+end_src"
                  (org-autodoc-annotate-with
                   "** "
                   'org-autodoc-annotate-as-org-list))
                 "\n\n"))
      ;; (set-text-properties 0 (length str) nil str)
      (when output-file
        (write-region str nil output-file nil nil))
      str)))

;;;###autoload
(defun org-autodoc-async ()
  "Show annotations as org list items."
  (interactive)
  (let* ((temp-file (make-temp-file (concat "annotate-"
                                            (file-name-base
                                             buffer-file-name))))
         (command (org-autodoc-get-emacs-batch-cmd
                   (format "(org-autodoc-org-annotation \"%s\")"
                           temp-file))))
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
             (kill-buffer (process-buffer process))
             (if (= (process-exit-status process) 0)
                 (progn
                   (find-file temp-file)
                   (org-mode))
               (user-error (format "%s\n%s" command output))))))
        (require 'comint)
        (when (fboundp 'comint-output-filter)
          (set-process-filter proc #'comint-output-filter))))))

(provide 'org-autodoc)
;;; org-autodoc.el ends here