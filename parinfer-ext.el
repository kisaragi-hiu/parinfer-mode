;;; parinfer-ext.el --- Extensions of parinfer-mode  -*- lexical-binding: t; -*-

;; Copyright (c) 2016, Shi Tianshu

;; Author: Shi Tianshu
;; Homepage: https://github.com/DogLooksGood/parinfer-mode
;; Keywords: Parinfer

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Extensions of parinfer

;;; Code:

(require 'cl-lib)
(require 'parinfer-strategies)

(defvar parinfer-extensions
  '(defaults pretty-parens smart-yank)
  "Parinfer extensions, which will be enabled when run parinfer.")

(defconst parinfer--extension-prefix "parinfer-ext::"
  "The prefix of parinfer extensions.")

;; -----------------------------------------------------------------------------
;; Definition
;; -----------------------------------------------------------------------------

(defun parinfer--plist2alist (plist)
  "Convert a property PLIST to an association list."
  (let (key output)
    (dolist (x plist)
      (if (keywordp x)
          (progn (setq key x)
                 (push (list key) output))
        (push `(,@(assq key output) ,x) output)))
    output))

(defmacro parinfer-define-extension (name doc-str &rest clauses)
  "Define an extension.

Extensions listed in `parinfer-extensions' are called on
different triggers (lifecycles).

Usage:
\(parinfer-define-extension NAME
  DOC-STR
  CLAUSES)

CLAUSES are the code for lifecycle.
:mount    called when 'parinfer-mode' enabled.
:unmount  called when 'parinfer-mode' disabled.
:paren    called when 'parinfer-mode' switch to Paren Mode.
:indent   called when 'parinfer-mode' switch to Indent Mode."
  (declare (indent 1) (doc-string 2))
  (let* ((alist (parinfer--plist2alist clauses))
         (keys (delete-dups (mapcar #'car alist)))
         (name-str (symbol-name name))
         clause)
    `(defun ,(intern (concat parinfer--extension-prefix name-str))
         (lifecycle)
       ,doc-str
       (cond
        ,@(cl-loop for key in keys
                   collect `((eq lifecycle ,key)
                             ,@(cdr (assq key alist))))))))

(defun parinfer--extension-funcall (extension lifecycle)
  "For specified EXTENSION, call its LIFECYCLE function."
  (let ((func (intern (concat parinfer--extension-prefix
                              (symbol-name extension)))))
    (when (functionp func)
      (funcall func lifecycle))))

(defun parinfer--extension-lifecycle (lifecycle)
  "Execute LIFECYCLE function for `parinfer-extensions'."
  (dolist (extension parinfer-extensions)
    (parinfer--extension-funcall extension lifecycle)))

;; -----------------------------------------------------------------------------
;; Pretty Parens
;; -----------------------------------------------------------------------------

(defun parinfer-pretty-parens:fontify-search (limit)
  (let ((result nil)
        (finish nil)
        (bound (+ (point) limit)))
    (while (not finish)
      (if (re-search-forward "\\s)" bound t)
          (when (and (= 0 (string-match-p "\\s)*$" (buffer-substring-no-properties (point) (line-end-position))))
                     (not (eq (char-before (1- (point))) 92)))
            (setq result (match-data)
                  finish t))
        (setq finish t)))
    result))

(defun parinfer-pretty-parens:refresh ()
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))

(defface parinfer-pretty-parens:dim-paren-face
  '((((class color) (background dark))
     (:foreground "grey40"))
    (((class color) (background light))
     (:foreground "grey60")))
  "Parinfer dim paren face."
  :group 'parinfer-faces)


(parinfer-define-extension pretty-parens
  "Pretty parens.

Use rainbow-delimiters for Paren Mode, and dim-style parens for Indent Mode."
  :mount
  (require 'font-lock)

  :paren
  (font-lock-remove-keywords
   nil '((parinfer-pretty-parens:fontify-search . 'parinfer-pretty-parens:dim-paren-face)))
  (when (fboundp 'rainbow-delimiters-mode)
    (rainbow-delimiters-mode-enable))
  (parinfer-pretty-parens:refresh)

  :indent
  (when (bound-and-true-p rainbow-delimiters-mode)
    (rainbow-delimiters-mode-disable))
  (font-lock-add-keywords
   nil '((parinfer-pretty-parens:fontify-search . 'parinfer-pretty-parens:dim-paren-face)))
  (parinfer-pretty-parens:refresh))

;; -----------------------------------------------------------------------------
;; defaults
;; -----------------------------------------------------------------------------

(defun parinfer-defaults:company-cancel (&ignored)
  "Invoke when company cancelled, ignore IGNORED."
  (parinfer-readjust-paren t))

(defun parinfer-defaults:company-finish (&ignored)
  "Invoke when company finished, ignore IGNORED. "
  (parinfer--reindent-sexp))

(parinfer-define-extension defaults
  "Basic compatibility fix."
  :indent
  (when (bound-and-true-p company-mode)
    (add-hook 'company-completion-cancelled-hook
              'parinfer-defaults:company-cancel t t)
    (remove-hook 'company-completion-finished-hook
                 'parinfer-defaults:company-finish t))
  :paren
  (when (bound-and-true-p company-mode)
    (add-hook 'company-completion-finished-hook
              'parinfer-defaults:company-finish t t)
    (remove-hook 'company-completion-cancelled-hook
                 'parinfer-defaults:company-cancel t)))

;; -----------------------------------------------------------------------------
;; paredit
;; -----------------------------------------------------------------------------

(parinfer-define-extension paredit
  "Introduce some paredit commands from paredit-mode."
  :mount
  (when (fboundp 'paredit-mode)
    (define-key parinfer-mode-map (kbd "C-{") 'paredit-backward-barf-sexp)
    (define-key parinfer-mode-map (kbd "C-}") 'paredit-forward-barf-sexp)
    (define-key parinfer-mode-map (kbd "C-(") 'paredit-backward-slurp-sexp)
    (define-key parinfer-mode-map (kbd "C-)") 'paredit-forward-slurp-sexp)
    (define-key parinfer-mode-map (kbd "M-r") 'paredit-raise-sexp)
    (define-key parinfer-mode-map (kbd "M-j") 'paredit-join-sexps)
    (define-key parinfer-mode-map (kbd "M-s") 'paredit-splice-sexp)
    (define-key parinfer-mode-map (kbd "M-S") 'paredit-split-sexp)))

;; -----------------------------------------------------------------------------
;; lispy
;; -----------------------------------------------------------------------------

(defun parinfer-lispy:space ()
  (interactive)
  (if (or (eq (point) (line-beginning-position))
          (eq (point) (line-end-position)))
      (call-interactively 'self-insert-command)
    (progn
      (call-interactively 'self-insert-command)
      (when (parinfer-lispy:paren-left-and-between-parens-p)
        (backward-char)))))

(defun parinfer-lispy:forward ()
  (interactive)
  (when parinfer--delay-timer
    (parinfer--clean-up))
  (call-interactively 'lispy-forward))

(defun parinfer-lispy:backward ()
  (interactive)
  (when parinfer--delay-timer
    (parinfer--clean-up))
  (call-interactively 'lispy-backward))

(defun parinfer-lispy:paren-char-p (c)
  (or (eq c 40)
      (eq c 91)
      (eq c 123)))

(defun parinfer-lispy:newline ()
  (interactive)
  (parinfer-do
   (call-interactively 'newline-and-indent)))

(defun parinfer-lispy:paren-left-and-between-parens-p ()
  (let ((ca (char-after))
        (cb (char-before (- (point) 1))))
    (and (not (parinfer--in-comment-or-string-p))
         (parinfer-lispy:paren-char-p ca)
         (parinfer-lispy:paren-char-p cb))))

(defun parinfer-lispy:parens (arg)
  (interactive "P")
  (if (or (region-active-p) arg)
      (call-interactively 'lispy-parens)
    (call-interactively 'self-insert-command)))

(defun parinfer-lispy:brackets (arg)
  (interactive "P")
  (if (or (region-active-p) arg)
      (call-interactively 'lispy-brackets)
    (call-interactively 'self-insert-command)))

(defun parinfer-lispy:braces (arg)
  (interactive "P")
  (if (or (region-active-p) arg)
      (call-interactively 'lispy-braces)
    (call-interactively 'self-insert-command)))

(defun parinfer-lispy:init ()
  (if (fboundp 'lispy-mode)
      (progn
        (mapc (lambda (x) (lispy-define-key parinfer-mode-map (format "%d" x) 'digit-argument))
              (number-sequence 0 9))
        (define-key parinfer-mode-map (kbd "(") 'parinfer-lispy:parens)
        (define-key parinfer-mode-map (kbd "{") 'parinfer-lispy:braces)
        (define-key parinfer-mode-map (kbd "[") 'parinfer-lispy:brackets)
        (define-key parinfer-mode-map (kbd "d") 'special-lispy-different)
        (define-key parinfer-mode-map (kbd "-") 'special-lispy-ace-subword)
        (define-key parinfer-mode-map (kbd "q") 'special-lispy-ace-paren)
        (define-key parinfer-mode-map (kbd "a") 'special-lispy-ace-symbol)
        (define-key parinfer-mode-map (kbd "c") 'special-lispy-clone)
        (define-key parinfer-mode-map (kbd "A") 'special-lispy-beginning-of-defun)
        (define-key parinfer-mode-map (kbd "w") 'special-lispy-move-up)
        (define-key parinfer-mode-map (kbd "s") 'special-lispy-move-down)
        (define-key parinfer-mode-map (kbd "h") 'special-lispy-left)
        (define-key parinfer-mode-map (kbd "l") 'special-lispy-right)
        (define-key parinfer-mode-map (kbd "k") 'special-lispy-up)
        (define-key parinfer-mode-map (kbd "j") 'special-lispy-down)
        (define-key parinfer-mode-map (kbd "u") 'special-lispy-undo)
        (define-key parinfer-mode-map (kbd "_") 'special-lispy-underscore)
        (define-key parinfer-mode-map (kbd "m") 'special-lispy-mark-list)
        (define-key parinfer-mode-map (kbd "v") 'special-lispy-view)
        (define-key parinfer-mode-map (kbd "M-m") 'lispy-mark-symbol)
        (define-key parinfer-mode-map (kbd "b") 'special-lispy-back)
        (define-key parinfer-mode-map (kbd "f") 'special-lispy-flow)
        (define-key parinfer-mode-map (kbd "e") 'special-lispy-eval)
        (define-key parinfer-mode-map (kbd "o") 'special-lispy-other-mode)
        (define-key parinfer-mode-map (kbd "O") 'special-lispy-oneline)
        (define-key parinfer-mode-map (kbd "M") 'special-lispy-alt-multiline)
        (define-key parinfer-mode-map (kbd "y") 'special-lispy-occur)
        (define-key parinfer-mode-map (kbd "i") 'special-lispy-tab)
        (define-key parinfer-mode-map (kbd "r") 'special-lispy-raise)
        (define-key parinfer-mode-map (kbd "C-a") 'lispy-move-beginning-of-line)
        (define-key parinfer-mode-map (kbd "g") 'special-lispy-goto)
        (define-key parinfer-mode-map (kbd ">") 'special-lispy-slurp)
        (define-key parinfer-mode-map (kbd "<") 'special-lispy-barf)
        (define-key parinfer-mode-map (kbd "n") 'special-lispy-new-copy)
        (define-key parinfer-mode-map (kbd "t") 'special-lispy-teleport)
        (define-key parinfer-mode-map (kbd "H") 'special-lispy-ace-symbol-replace)
        (define-key parinfer-mode-map (kbd "p") 'special-lispy-paste)
        (define-key parinfer-mode-map (kbd "/") 'special-lispy-splice)
        (define-key parinfer-mode-map (kbd "SPC") 'parinfer-lispy:space))
    (message "Parinfer extension lispy: It seems Lispy is not installed!")))

(parinfer-define-extension lispy
  "Integration with Lispy."
  :mount
  (require 'eldoc)
  (eldoc-add-command-completions "lispy-" "parinfer-")
  (parinfer-strategy-add :default
    #'parinfer-lispy:parens
    #'parinfer-lispy:braces
    #'parinfer-lispy:brackets
    #'parinfer-lispy:space)
  (parinfer-strategy-add :instantly
    #'newline)
  (parinfer-lispy:init))

;; -----------------------------------------------------------------------------
;; Smart yank
;; -----------------------------------------------------------------------------

(defun parinfer-smart-yank:paren-yank ()
  (interactive)
  (let ((yank-str nil)
        (m major-mode))
    (with-temp-buffer
      (yank)
      (funcall m)
      (ignore-errors (parinfer--reindent-sexp))
      (parinfer-readjust-paren-buffer)
      (setq yank-str (buffer-substring-no-properties (point-min) (point-max))))
    (parinfer-paren-run
     (insert yank-str)
     (parinfer--reindent-sexp))))

(defun parinfer-smart-yank:yank ()
  "Yank behaviour depend on current mode(Indent/Paren)."
  (interactive)
  (cl-case parinfer--mode
    ('indent (call-interactively 'parinfer-yank))
    ('paren (call-interactively 'parinfer-smart-yank:paren-yank))))

(parinfer-define-extension smart-yank
  "Yank depend on current mode."
  :mount
  (define-key parinfer-mode-map [remap yank] 'parinfer-smart-yank:yank))

;; -----------------------------------------------------------------------------
;; Smart TAB
;; -----------------------------------------------------------------------------

(defconst parinfer-smart-tab:close-paren-regex "\\()\\|]\\|}\\)")

(defvar-local parinfer-smart-tab:indicator-line nil)

(defface parinfer-smart-tab:indicator-face
  '((((class color) (background dark))
     (:background "grey40"))
    (((class color) (background light))
     (:background "grey60")))
  "Parinfer Smart TAB indicator."
  :group 'parinfer-faces)

(defun parinfer-smart-tab:clean-not-skip-this-command-p ()
  (and (symbolp this-command)
       (not (eq this-command 'parinfer-smart-tab:dwim-right-or-complete))
       (not (eq this-command 'parinfer-smart-tab:dwim-right))
       (not (eq this-command 'parinfer-smart-tab:dwim-left))
       (not (eq this-command 'parinfer-smart-tab:forward-char))
       (not (eq this-command 'parinfer-smart-tab:backward-char))
       (not (eq this-command 'parinfer-smart-tab:forward-char-with-indicator))
       (not (eq this-command 'parinfer-smart-tab:backward-char-with-indicator))))

(defun parinfer-smart-tab:clean-indicator-pre ()
  (interactive)
  (when (and parinfer-smart-tab:indicator-line
             (parinfer-smart-tab:clean-not-skip-this-command-p))
    (save-excursion
      (parinfer--goto-line parinfer-smart-tab:indicator-line)
      (remove-text-properties
       (line-beginning-position)
       (line-end-position)
       '(font-lock-face 'parinfer-smart-tab:indicator-face)))))

(defun parinfer-smart-tab:clean-indicator ()
  (interactive)
  (when (and parinfer-smart-tab:indicator-line
             (parinfer-smart-tab:clean-not-skip-this-command-p))
    (if (and (eq (line-number-at-pos) parinfer-smart-tab:indicator-line))
        (save-excursion
          (end-of-line)
          (while (eq (char-before) 32)
            (backward-delete-char 1)))
      (save-excursion
        (parinfer--goto-line parinfer-smart-tab:indicator-line)
        (when (parinfer--empty-line-p)
          (delete-region (line-beginning-position) (line-end-position)))))
    (setq parinfer-smart-tab:indicator-line nil)))

(defun parinfer-smart-tab:region-x-and-positions ()
  (let ((begin (region-beginning))
        (end (region-end))
        (m major-mode))
    (setq parinfer--region-shifted nil)
    (deactivate-mark)
    (let* ((sexp-begin (save-excursion (goto-char begin)
                                       (parinfer--goto-previous-toplevel)
                                       (point)))
           (text (buffer-substring-no-properties sexp-begin begin))
           (pos-list nil))
      (progn
        (with-temp-buffer
          (insert text)
          (newline-and-indent)
          (parinfer-readjust-paren-buffer)
          (funcall m)
          (setq pos-list (parinfer-smart-tab:find-possible-positions))))
      (goto-char begin)
      (back-to-indentation)
      (let ((x (- (point) (line-beginning-position))))
        (goto-char begin)
        (set-mark-command nil)
        (goto-char end)
        (list x pos-list)))))

(defun parinfer-smart-tab:shift (distance)
  "Shift text.  For right, DISTANCE > 0; left, DISTANCE < 0."
  (parinfer-silent
   (when (use-region-p)
     (let ((mark (mark)))
       (save-excursion
         (indent-rigidly (region-beginning)
                         (region-end)
                         distance)
         (push-mark mark t t)
         (setq parinfer--x-after-shift
               (+ parinfer--x-after-shift distance))
         (setq deactivate-mark nil))))))

(defun parinfer-smart-tab:active-line-region ()
  "Auto adjust region so that the shift can work properly."
  (setq parinfer--x-after-shift (- (point) (line-beginning-position)))
  (let* ((begin (region-beginning))
         (end (region-end))
         (new-begin (save-excursion
                      (goto-char begin)
                      (line-beginning-position))))
    (goto-char new-begin)
    (set-mark-command nil)
    (goto-char end)
    (end-of-line)
    (setq deactivate-mark nil)))

(defun parinfer-smart-tab:shift-right ()
  (interactive)
  (if (eq 'indent parinfer--mode)
      (progn
        (parinfer-smart-tab:active-line-region)
        (let* ((x-and-pos-list (parinfer-smart-tab:region-x-and-positions))
               (x (car x-and-pos-list))
               (pos-list (cadr x-and-pos-list))
               (pos-list-1 (-filter (lambda (n) (> n x))
                                    pos-list)))
          (if (not pos-list-1)
              (parinfer-smart-tab:shift (- x))
            (let ((min-x (-min pos-list-1)))
              (parinfer-smart-tab:shift (- min-x x))))
          (setq parinfer--region-shifted t)))
    (progn
      (setq deactivate-mark t)
      (parinfer--reindent-sexp))))

(defun parinfer-smart-tab:shift-left ()
  (interactive)
  (if (eq 'indent parinfer--mode)
      (progn
        (parinfer-smart-tab:active-line-region)
        (let* ((x-and-pos-list (parinfer-smart-tab:region-x-and-positions))
               (x (car x-and-pos-list))
               (pos-list (cadr x-and-pos-list))
               (pos-list-1 (-filter (lambda (n) (< n x))
                                    pos-list)))
          (if (not pos-list-1)
              (parinfer-smart-tab:shift (- (-max pos-list) x))
            (let ((max-x (-max pos-list-1)))
              (parinfer-smart-tab:shift (- max-x x))))
          (setq parinfer--region-shifted t)))
    (progn
      (setq deactivate-mark t)
      (parinfer--reindent-sexp))))

(defun parinfer-smart-tab:at-close-paren-p ()
  (let ((c (char-after)))
    (or (eq c 41)
        (eq c 93)
        (eq c 125))))

(defun parinfer-smart-tab:should-ignore-positions (pos-list)
  (or (not pos-list)
      (= 1 (length pos-list))))

(defun parinfer-smart-tab:find-possible-positions ()
  (save-excursion
    (unless (= 1 (line-number-at-pos))
      (forward-line -1)
      (while (and (or (parinfer--comment-line-p)
                      (parinfer--empty-line-p))
                  (not (= 1 (line-number-at-pos))))
        (forward-line -1))
      (unless (or (parinfer--comment-line-p)
                  (parinfer--empty-line-p))
        (end-of-line)
        (let ((pos-list (list 0)))
          (newline-and-indent)
          (cl-pushnew (- (point) (line-beginning-position)) pos-list)
          (delete-indentation)
          (backward-char)
          (while (parinfer-smart-tab:at-close-paren-p)
            (newline-and-indent)
            (cl-pushnew (- (point) (line-beginning-position)) pos-list)
            (delete-indentation)
            (backward-char))
          (end-of-line)
          (while (eq 32 (char-before))
            (backward-delete-char 1))
          pos-list)))))

(defun parinfer-smart-tab:mark-positions (pos-list)
  (unless (parinfer-smart-tab:should-ignore-positions pos-list)
    (setq parinfer-smart-tab:indicator-line (line-number-at-pos))
    (let ((ln (car pos-list))
          (current-x (- (point) (line-beginning-position))))
      (delete-region (line-beginning-position) (line-end-position))
      (cl-loop for i from 0 to ln do
               (if (memql i pos-list)
                   (insert (propertize " " 'font-lock-face 'parinfer-smart-tab:indicator-face))
                 (insert " ")))
      (beginning-of-line)
      (if (> current-x ln)
          (progn (end-of-line) (backward-char))
        (forward-char current-x)))))

(defun parinfer-smart-tab:forward-char ()
  (interactive)
  (if (and (not (parinfer--in-comment-or-string-p))
           (parinfer--empty-line-p))
      (progn
        (when parinfer--delay-timer
          (parinfer--clean-up))
        (let ((pos-list (parinfer-smart-tab:find-possible-positions))
              (current-x (- (point) (line-beginning-position))))
          (if (>= current-x (-max pos-list))
              (progn
                (beginning-of-line)
                (delete-region (point) (line-end-position)))
            (progn
              (beginning-of-line)
              (let ((next-x (-last-item (-filter (lambda (x) (> x current-x)) pos-list))))
                (cl-loop for i from 1 to next-x do
                         (insert " ")))))
          (setq parinfer-smart-tab:indicator-line (line-number-at-pos))))
    (call-interactively 'forward-char)))

(defun parinfer-smart-tab:backward-char ()
  (interactive)
  (if (and (not (parinfer--in-comment-or-string-p))
           (parinfer--empty-line-p))
      (progn
        (when parinfer--delay-timer
          (parinfer--clean-up))
        (let ((pos-list (parinfer-smart-tab:find-possible-positions))
              (current-x (- (point) (line-beginning-position))))
          (if (<= current-x (-min pos-list))
              (progn
                (beginning-of-line)
                (let ((next-x (-max pos-list)))
                  (cl-loop for i from 1 to next-x do
                           (insert " "))))
            (progn
              (beginning-of-line)
              (let ((next-x (-first-item (-filter (lambda (x) (< x current-x)) pos-list))))
                (cl-loop for i from 1 to next-x do
                         (insert " ")))))
          (setq parinfer-smart-tab:indicator-line (line-number-at-pos))))
    (call-interactively 'backward-char)))

(defun parinfer-smart-tab:forward-char-with-indicator ()
  (interactive)
  (when (and (not (parinfer--in-comment-or-string-p))
             (parinfer--empty-line-p)
             (not parinfer-smart-tab:indicator-line))
    (when parinfer--delay-timer
      (parinfer--clean-up))
    (let ((pos-list (parinfer-smart-tab:find-possible-positions)))
      (parinfer-smart-tab:mark-positions pos-list)))
  (if (and parinfer-smart-tab:indicator-line
           (eq (line-number-at-pos) parinfer-smart-tab:indicator-line))
      (progn
        (if (eq (point) (line-end-position))
            (beginning-of-line)
          (forward-char))
        (let ((forward-count 0))
          (while (and (not (eq (face-at-point) 'parinfer-smart-tab:indicator-face))
                      (< forward-count 300))
            (if (eq (point) (line-end-position))
                (beginning-of-line)
              (forward-char))
            (setq forward-count (1+ forward-count)))))
    (call-interactively 'forward-char)))

(defun parinfer-smart-tab:backward-char-with-indicator ()
  (interactive)
  (when (and (not (parinfer--in-comment-or-string-p))
             (parinfer--empty-line-p)
             (not parinfer-smart-tab:indicator-line))
    (when parinfer--delay-timer
      (parinfer--clean-up))
    (let ((pos-list (parinfer-smart-tab:find-possible-positions)))
      (parinfer-smart-tab:mark-positions pos-list)))
  (if (and parinfer-smart-tab:indicator-line
           (eq (line-number-at-pos) parinfer-smart-tab:indicator-line))
      (progn
        (if (eq (point) (line-beginning-position))
            (end-of-line)
          (backward-char))
        (let ((backward-count 0))
          (while (and (not (eq (face-at-point) 'parinfer-smart-tab:indicator-face))
                      (< backward-count 300))
            (if (eq (point) (line-beginning-position))
                (end-of-line)
              (backward-char))
            (setq backward-count (1+ backward-count)))))
    (call-interactively 'backward-char)))

(defun parinfer-smart-tab:dwim-right-or-complete ()
  (interactive)
  (if (eq 'paren parinfer--mode)
      (if (bound-and-true-p company-mode)
          (company-indent-or-complete-common)
        (indent-according-to-mode))
    (cond
     ((region-active-p)
      (call-interactively 'parinfer-smart-tab:shift-right))

     ((parinfer--empty-line-p)
      (parinfer-smart-tab:forward-char))

     ((and (bound-and-true-p company-mode)
           (looking-at "\\_>"))
      (company-complete-common))

     (t
      (progn
        (call-interactively 'set-mark-command)
        (activate-mark)
        (call-interactively 'parinfer-smart-tab:shift-right)
        (deactivate-mark))))))

(defun parinfer-smart-tab:dwim-right ()
  (interactive)
  (if (eq 'paren parinfer--mode)
      (indent-according-to-mode)
    (cond
     ((region-active-p)
      (call-interactively 'parinfer-smart-tab:shift-right))

     ((parinfer--empty-line-p)
      (parinfer-smart-tab:forward-char))

     (t
      (progn
        (call-interactively 'set-mark-command)
        (activate-mark)
        (call-interactively 'parinfer-smart-tab:shift-right)
        (deactivate-mark))))))

(defun parinfer-smart-tab:dwim-left ()
  (interactive)
  (when (eq 'indent parinfer--mode)
    (cond
     ((region-active-p)
      (call-interactively 'parinfer-smart-tab:shift-left))

     ((parinfer--empty-line-p)
      (parinfer-smart-tab:backward-char))

     (t
      (progn
        (call-interactively 'set-mark-command)
        (activate-mark)
        (call-interactively 'parinfer-smart-tab:shift-left)
        (deactivate-mark))))))

(parinfer-define-extension smart-tab
  "Smart forward-char & backward-char."
  :mount
  (add-hook 'post-command-hook 'parinfer-smart-tab:clean-indicator t t)
  (add-hook 'pre-command-hook 'parinfer-smart-tab:clean-indicator-pre t t)
  (define-key parinfer-mode-map [remap forward-char] 'parinfer-smart-tab:forward-char)
  (define-key parinfer-mode-map [remap backward-char] 'parinfer-smart-tab:backward-char)
  (define-key parinfer-region-mode-map [remap parinfer-shift-right] 'parinfer-smart-tab:shift-right)
  (define-key parinfer-region-mode-map [remap parinfer-shift-left] 'parinfer-smart-tab:shift-left)
  ;; FIXME: if region is active, we won't be in insert state...
  (if (fboundp 'evil-define-key*)
      (progn (evil-define-key* 'insert parinfer-region-mode-map
               [remap evil-shift-right-line] 'parinfer-smart-tab:shift-right)
             (evil-define-key* 'insert parinfer-region-mode-map
               [remap evil-shift-left-line] 'parinfer-smart-tab:shift-left)))

  :unmount
  (remove-hook 'post-command-hook 'parinfer-smart-tab:clean-indicator t)
  (remove-hook 'pre-command-hook 'parinfer-smart-tab:clean-indicator-pre t))

(provide 'parinfer-ext)
;;; parinfer-ext.el ends here
