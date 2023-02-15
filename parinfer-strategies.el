;;; parinfer-strategies.el --- Strageties -*- lexical-binding: t -*-

;;; Commentary:

;; Entry point: parinfer-strategy-match-p

;;; Code:

(require 'cl-lib)

(defvar parinfer-strategies (make-hash-table)
  "Parinfer invoke strategy.

Use `parinfer-strategy-add' to mark functions as using a
particular strategy.

This is a hash table with values being the commands, and keys
being the following:

 strategy name    Description
 --------------   -------------------------------------------
 :default         readjust indent or paren (delay on large sexp)
 :instantly       readjust indent or paren
 :shift-right     make evil-shift-right work more as expected
 :skip            do not invoke parinfer

The values are either symbols or regexp strings that are used to
match commands.")

(defun parinfer-strategy-add (strategy &rest commands)
  "Append new commands to STRATEGY in `parinfer-strategy'.

COMMANDS is a list of commands, which may be a symbol or a regexp
string."
  (declare (indent 1))
  (unless (hash-table-p parinfer-strategies)
    (setq parinfer-strategies (make-hash-table)))
  (dolist (cmd commands)
    (cl-pushnew
     cmd
     (gethash strategy parinfer-strategies)
     :test #'equal)))

(parinfer-strategy-add :default
  'evil-shift-left
  'evil-shift-left-line)
(parinfer-strategy-add :shift-right
  'evil-shift-right
  'evil-shift-right-line)
(parinfer-strategy-add :default
  "paredit-"
  'comment-dwim
  'comment-line
  'comment-or-uncomment-region
  'delete-char
  'delete-indentation
  'kill-line
  'kill-region
  'kill-word
  'newline-and-indent
  'sp-insert-pair
  'evil-commentary)
(parinfer-strategy-add :instantly
  'indent-for-tab-command
  'parinfer-double-quote
  'outshine-self-insert-command
  'delete-region
  'self-insert-command
  'newline
  'evil-join
  'evil-replace
  'evil-change
  'evil-change-line
  'evil-change-whole-line
  'evil-delete
  'evil-delete-backward-char
  'evil-delete-char
  'evil-delete-line
  'evil-exit-visual-state
  'evil-force-normal-state
  'evil-normal-state
  'evil-paste-after
  'evil-paste-before
  'evil-substitute)
(parinfer-strategy-add :skip
  'evil-previous-line
  'evil-forward-char
  'evil-backward-char
  'evil-next-line
  'evil-forward-word
  'evil-forward-word-begin
  'evil-backward-word-begin
  'evil-backward-end
  'evil-scroll-page-down
  'evil-scroll-up)

(defun parinfer-strategy-match-p (command strategy)
  "Return non-nil if COMMAND's parinfer invoke strategy is STRATEGY."
  (let ((cmds (gethash strategy parinfer-strategies)))
    (or (memq command cmds)
        (cl-some (lambda (regexp)
                   (and (stringp regexp)
                        (string-match-p regexp
                                        (symbol-name command))))
                 cmds))))
        ;; (and (eq strategy :default)
        ;;      (seq-every-p
        ;;       (lambda (key)
        ;;         (not (parinfer-strategy-match-p command key)))
        ;;       (remq :default
        ;;             (hash-table-keys parinfer-strategies)))))))

(provide 'parinfer-strategies)

;;; parinfer-strategies.el ends here
