;;; parinfer-strategies.el --- Strageties -*- lexical-binding: t -*-

;;; Commentary:

;; Entry point: parinfer-strategy-match-p

;;; Code:

(defvar parinfer-strategies nil
  "Parinfer invoke strategy.

Use `parinfer-strategy-add' to mark functions as using a
particular strategy. Use `parinfer-strategy-parse' to read
values.

This is a plist with values being the commands, and keys being
the following:

 strategy name    Description
 --------------   -------------------------------------------
 :default         readjust indent or paren (delay on large sexp)
 :instantly       readjust indent or paren
 :shift-right     make evil-shift-right work more as expected
 :skip            do not invoke parinfer

The values are either symbols or regexp strings that are used to
match commands.")

(defun parinfer-strategy-parse (strategy)
  "Return commands in `parinfer-strategies' matching STRATEGY.

Return a plist like:

 (:commands cmd1 cmd2 cmd3
  :regexps regexp1 regexp2 regexp3)"
  (let ((lst (plist-get parinfer-strategies strategy)))
    (list :commands (cl-remove-if-not #'symbolp lst)
          :regexps  (cl-remove-if-not #'stringp lst))))

(defun parinfer-strategy-add (strategy &rest commands)
  "Append new commands to STRATEGY in `parinfer-strategy'.

COMMANDS is a list of commands, which may be a symbol or a regexp
string."
  (declare (indent 1))
  (dolist (cmd commands)
    (cl-pushnew
     cmd
     (plist-get parinfer-strategies strategy)
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

(defun parinfer-strategy-match-p (command strategy-name)
  "Return non-nil if COMMAND's parinfer invoke strategy is STRATEGY-NAME."
  (let* ((output (parinfer-strategy-parse strategy-name))
         (cmds (plist-get output :commands))
         (regexps (plist-get output :regexps)))
    (or (member command cmds)
        (cl-some (lambda (regexp)
                   (string-match-p
                    regexp (symbol-name command)))
                 regexps))))

(provide 'parinfer-strategies)

;;; parinfer-strategies.el ends here
