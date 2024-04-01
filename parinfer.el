;;; parinfer.el --- Simpler Lisp editing  -*- lexical-binding: t; -*-

;; Copyright (c) 2016, Shi Tianshu

;; Author: Shi Tianshu
;; Homepage: https://github.com/DogLooksGood/parinfer-mode
;; Version: 0.6.0
;; Package-Requires: ((emacs "25.1") (dash "2.13.0") (cl-lib "0.5"))
;; Keywords: convenience, parinfer

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

;; Credits:
;;
;; - Shi Tianshu <https://github.com/DogLooksGood>, creator of parinfer-mode
;;
;; Original credits:
;;
;; - [[https://github.com/shaunlebron][shaunlebron]] :: Create Parinfer.
;; - [[https://github.com/oakmac][oakmac]] :: Bring Parinfer to Emacs.
;; - [[https://github.com/tumashu][tumashu]] :: Help me a lot in writing this plugin.
;; - [[https://github.com/purcell][purcell]] & [[https://github.com/syohex][syohex]] :: Advice and Tips for writing Emacs plugin.
;;
;; License: GPLv3.

;;; Code:
;;;; Requires
(require 'cl-lib)
(require 'dash)
(require 'mode-local)

;; for parinfer-diff
(require 'ediff)

;; Internal
(require 'parinferlib)
(require 'parinfer-utils)
(require 'parinfer-ext)
(require 'parinfer-strategies)

;;;; User options

(defgroup parinfer nil
  "Parinfer."
  :group 'lisp)

(defgroup parinfer-faces nil
  "Parinfer faces."
  :group 'faces)

(defcustom parinfer-change-to-indent-state-automatically nil
  "Switch back to indent state automatically if parens are balanced.

nil: never
t: after every command if parens are balanced
`closing': only after inserting a closing paren"
  :group 'parinfer
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Whenever parens are balanced" t)
                 (const :tag "Only after inserting a close paren" closing)))

(defcustom parinfer-lighters
  '("Parinfer:Indent" . "Parinfer:Paren")
  "Parinfer lighters in the mode line.

The car is shown in the mode line in indent state; the cdr in
paren state."
  :group 'parinfer
  :type '(cons (string :tag "Indent state lighter")
               (string :tag "Paren state lighter")))

(defcustom parinfer-preview-cursor-scope nil
  "Whether to show cursor scope in indent state.

The cursor's scope will be shown on an empty line by having close
parens inserted after it."
  :group 'parinfer
  :type 'boolean)

(defcustom parinfer-delay-invoke-threshold 6000
  "The character threshold for delayed processing.

Delayed processing is used when more than this number of
characters have been changed."
  :group 'parinfer
  :type 'number)

(defcustom parinfer-delay-invoke-idle 0.3
  "The amount of idle time in seconds for delayed processing."
  :group 'parinfer
  :type 'number)

(defcustom parinfer-display-error t
  "Whether indent state errors should be displayed."
  :group 'parinfer
  :type 'boolean)

(defcustom parinfer-extensions
  '(defaults pretty-parens smart-yank)
  "Parinfer extensions, which will be enabled when run parinfer."
  :group 'parinfer
  :type '(repeat symbol))

(defvar parinfer-mode-enable-hook nil
  "Called after `parinfer-mode' is enabled.")

(defvar parinfer-mode-disable-hook nil
  "Called after `parinfer-mode' is disabled.")

(defvar parinfer-change-state-functions nil
  "Called after changing to indent state or paren state.

The functions are passed one argument, STATE, which is either
`indent' or `paren', for the state that was just switched to.")

(defvar parinfer-after-execute-hook nil
  "Called after `parinfer-readjust-paren'.")

;;;; Internal variables and constants

(defvar-local parinfer--state 'paren
  "The current parinfer state, either `paren' or `indent'.")

(defvar-local parinfer--first-load t
  "Whether parinfer is first loaded in the buffer.

In other words, if the buffer hasn't switched to indent state
yet.")

(defvar-local parinfer--text-modified nil
  "Whether the last command modified text.")

(defvar-local parinfer--last-line-number -1
  "Holds the last line number after `parinfer--invoke-if-necessary'.")

(defvar-local parinfer--delay-timer nil
  "Used to store the idle timer for delayed processing.")

(defvar-local parinfer--x-after-shift nil
  "Where the cursor x should be, after region has been shifted.")

(defvar-local parinfer--region-shifted nil
  "If shift the region after mark activate.")

(defvar-local parinfer--ppss nil
  "Used as a cache for `syntax-ppss' output.")

;;;; Helper macros

(defmacro parinfer-paren-run (&rest body)
  "Run BODY in paren state, ensuring indentation is correct afterwards."
  (let ((toggle (make-symbol "toggle")))
    `(let ((,toggle (eq parinfer--state 'indent)))
       (parinfer-silent
        (when ,toggle
          (parinfer--change-to-state 'paren))
        ,@body
        (when ,toggle
          (parinfer--reindent-sexp)
          (parinfer--change-to-state 'indent)
          (parinfer--invoke-if-necessary))))))

(defmacro parinfer-run (&rest body)
  "Run BODY, then invoke parinfer.

Reset delay if exists."
  `(progn
     ,@body
     (setq parinfer--text-modified t)
     (parinfer--invoke)))

(defmacro parinfer-do (&rest body)
  "Run BODY, then invoke parinfer.

Clean up delay if exists."
  `(progn
     (when parinfer--delay-timer
       (parinfer--clean-up))
     ,@body
     (setq parinfer--text-modified t)
     (parinfer--invoke)))

;;;; Helper functions

(defun parinfer--reindent-sexp ()
  "Reindent current sexp."
  (parinfer-silent
   (when (not (parinfer--in-comment-or-string-p))
     (let ((p (point-marker)))
       (set-marker-insertion-type p t)
       (indent-region
        (save-mark-and-excursion
          (beginning-of-defun 1) (point))
        (save-mark-and-excursion
          (end-of-defun 1) (point)))
       (goto-char p)))))

(defun parinfer--paren-balanced-p ()
  "Return if current sexp is parens-balanced."
  (save-mark-and-excursion
    (parinfer--goto-current-toplevel)
    (let ((old-point (point)))
      (ignore-errors (forward-sexp))
      (unless (eq old-point (point))
        (eq (point) (line-end-position))))))

(defun parinfer--unfinished-string-p ()
  "Whether there is an unclosed string in the buffer."
  (save-mark-and-excursion
    (goto-char (point-max))
    (parinfer--in-string-p)))

(defun parinfer--errmsg (line-number msg)
  "Report an error, described with MSG, that happened at LINE-NUMBER."
  (message
   (if line-number
       (format "Parinfer: Error on line %d: %s. Switching to paren state"
               line-number msg)
     (format "Parinfer: Error: %s. Switching to paren state"
             msg))))

;;;; Minor mode logic

(defun parinfer--init ()
  "Initialize parinfer mode.

Switch to paren state first. If readjusting parens would change
the buffer, stay in paren state; otherwise switch to indent
state."
  (parinfer--change-to-state 'paren)
  (pcase (parinfer--indent-changes)
    (`unchanged
     (parinfer--change-to-state 'indent))
    (`changed
     (message
      (substitute-command-keys
       (concat "Parinfer: Paren state, use \\[parinfer-toggle-state] "
               "to switch to Indent state."))))
    (`(error ,err)
     (parinfer--errmsg
      (plist-get err :line-no)
      (plist-get err :message)))))

(defun parinfer--change-to-state (state)
  "Change to STATE (`indent' or `paren').

This function only changes the state and does not readjust parens
or indent."
  (let ((kw (cond ((eq state 'indent) :indent)
                  ((eq state 'paren) :paren))))
    (unless kw
      (error "State must be `indent' or `paren'"))
    (setq parinfer--state state)
    (setq parinfer--first-load nil)
    (run-hook-with-args 'parinfer-change-state-functions state)
    (parinfer--extension-lifecycle kw)
    (force-mode-line-update)))

(defun parinfer--last-change-involves-newline ()
  "Check if the last change has a newline."
  (unless
      ;; No undo information recorded
      (eq buffer-undo-list t)
    (let ((undo-list buffer-undo-list))
      ;; remove consecutive boundrary markers
      (while (and undo-list (not (car undo-list)))
        (pop undo-list))
      (cl-loop
       for x in undo-list
       with str = nil
       if (or (not x) (not (consp x))) return nil

       ;; insertion between A and B
       if (and (integerp (car x))
               (integerp (cdr x)))
       do (setq str (buffer-substring-no-properties (car x) (cdr x)))

       ;; deletion of A (at B)
       if (stringp (car x)) do (setq str (car x))

       if (and str (string-match-p "\n" str)) return t
       else do (setq str nil)))))

(defun parinfer--in-comment-or-string-p ()
  "Return if we are in comment or string."
  (let ((f (get-text-property (point) 'face))
        (ppss (or parinfer--ppss (syntax-ppss))))
    (or (eq f 'font-lock-comment-face)
        (eq f 'font-lock-comment-delimiter-face)
        (nth 3 ppss)
        (nth 4 ppss))))

(defun parinfer--in-string-p ()
  "Return if we are in string."
  (nth 3 (or parinfer--ppss (syntax-ppss))))

(defun parinfer--goto-line (n)
  "Goto the beginning of line N."
  (goto-char (point-min))
  (forward-line (1- n))
  (beginning-of-line))

(defun parinfer--empty-line-p ()
  "Whether this line is empty or contains only whitespace."
  (or (eq (line-beginning-position) (line-end-position))
      (string-match-p
       "^[[:blank:]]+$"
       (buffer-substring-no-properties (line-beginning-position)
                                       (line-end-position)))))

(defun parinfer--toplevel-line-p ()
  "Return whether this line starts with a top-level statement."
  ;; FIXME: this breaks for a toplevel quoted list (but not for a
  ;; toplevel quasiquoted list).
  (string-match-p "^[({\\[#`]" (buffer-substring-no-properties
                                (line-beginning-position)
                                (line-end-position))))

(defun parinfer--goto-current-toplevel ()
  "Goto the beginning of current toplevel sexp."
  (back-to-indentation)
  (let ((prev-pos (point-max)))
    (while (and (not (eq (point) (point-min)))
                (not (eq (point) prev-pos))
                (or (not (eq (point) (line-beginning-position)))
                    (parinfer--empty-line-p)
                    (not (parinfer--toplevel-line-p))
                    (parinfer--in-comment-or-string-p)))
      (setq prev-pos (point))
      (forward-line -1)
      (back-to-indentation))
    ;; when insert parens after some whitespaces at first line,
    ;; we need consider the beginning of buffer as the begin of processing range.
    (when (eq prev-pos (point))
      (beginning-of-line))))

(defun parinfer--goto-next-toplevel ()
  "Goto the beggining of next toplevel sexp."
  (if (eq (line-end-position) (point-max))
      (end-of-line)
    (progn
      (forward-line 1)
      (let ((found nil))
        (while (not found)
          (if (eq (line-end-position) (point-max))
              (progn
                (end-of-line)
                (setq found t))
            (progn
              (back-to-indentation)
              (if (and (eq (point) (line-beginning-position))
                       (parinfer--toplevel-line-p)
                       (not (or (parinfer--empty-line-p)
                                (parinfer--in-comment-or-string-p))))
                  (progn
                    (beginning-of-line)
                    (setq found t))
                (forward-line 1)))))))))

(defun parinfer--goto-previous-toplevel ()
  "Goto the beggining of previous toplevel sexp."
  (parinfer--goto-current-toplevel)
  (forward-line -1)
  (parinfer--goto-current-toplevel))

(defun parinfer--lighter ()
  "Return the lighter for the current state."
  (let ((str (if (eq 'paren parinfer--state)
                 (cdr parinfer-lighters)
               (car parinfer-lighters))))
    ;; Add the space in the lighter automatically
    (if (equal (elt str 0) ?\s)
        str
      (concat " " str))))

(defun parinfer--ediff-init-keys ()
  "Inits for ediff.  since we don't need all features, simplify opeators."
  (local-set-key (kbd "q") 'parinfer-ediff-quit))

(defun parinfer--cursor-x ()
  "Return the cursor-x as required by parinferlib."
  (abs (- (line-beginning-position) (point))))

(defun parinfer--invoke-immediately (&optional pos)
  "Invoke parinfer immediately.

Readjust indentation in paren state; readjust parens in indent
state.

Paren readjustment is performed immediately, regardless of
`parinfer-delay-invoke-threshold'. (Indent readjustment never
uses delayed processing.)

POS is the position we want to call parinfer."
  (if (and pos (not (eq pos (point))))
      (let ((ln (line-number-at-pos))
            (x (parinfer--cursor-x)))
        (goto-char pos)
        (parinfer--invoke-immediately)
        (parinfer--goto-line ln)
        (forward-char x))
    (cond
     ((eq 'paren parinfer--state) (parinfer-readjust-indent))
     ((eq 'indent parinfer--state) (parinfer-readjust-paren))
     (t "nothing"))))

(defun parinfer--invoke (&optional pos)
  "Invoke Parinfer.

Readjust indentation in paren state, readjust parens in indent
state. Paren readjustment is delayed after
`parinfer-delay-invoke-idle' seconds of idle time if the text to
be changed is longer than `parinfer-delay-invoke-threshold'.

POS is the position we want to call parinfer."
  (if (and pos (not (eq pos (point))))
      (let ((current-pos (point)))
        (goto-char pos)
        (parinfer--invoke)
        (goto-char current-pos))
    (cond
     ((eq 'paren parinfer--state) (parinfer-readjust-indent))
     ((eq 'indent parinfer--state) (parinfer-readjust-paren t))
     (t "nothing"))))

(defun parinfer--should-clean-up-p ()
  "Should parinfer do clean job."
  (and (eq parinfer--state 'indent)
       parinfer--text-modified
       (not (equal parinfer--last-line-number (line-number-at-pos)))))

(defun parinfer--unsafe-p ()
  "If should prevent call parinfer absolutely."
  (or (bound-and-true-p multiple-cursors-mode)
      (use-region-p)
      (and (eql #x20 (char-before))
           (save-match-data
             (looking-at
              (rx (+ (syntax close-parenthesis))
                  eol))))))

(defun parinfer--clean-up ()
  "Clean up the delay timer and invoke parinfer immediately."
  (when parinfer--delay-timer
    (cancel-timer parinfer--delay-timer)
    (setq parinfer--delay-timer nil))
  (let ((pos (save-mark-and-excursion
               (parinfer--goto-line parinfer--last-line-number)
               (line-beginning-position))))
    (parinfer--invoke-immediately pos)))

(defun parinfer--comment-line-p ()
  "Return whether the current line is a comment."
  (save-mark-and-excursion
    (back-to-indentation)
    (let ((f (get-text-property (point) 'face)))
      (and (string-match-p "^;+.*$" (buffer-substring-no-properties (point) (line-end-position)))
           (save-mark-and-excursion
             (end-of-line)
             (or (eq f 'font-lock-comment-face)
                 (eq f 'font-lock-comment-delimiter-face)
                 (nth 4 (or parinfer--ppss (syntax-ppss)))))))))

(defun parinfer--should-invoke? ()
  "Is it necessary to invoke parinfer?"
  (and (not (parinfer-strategy-match-p this-command :skip))
       ;; This fixes pasting code starting with comments
       ;; not triggering a paren adjust.
       ;;
       ;; If there is a newline, we take that as a signal
       ;; that there might be code following the comment
       ;; that needs readjustment.
       ;;
       ;; Has newline or not in a comment = should run
       (or (parinfer--last-change-involves-newline)
           (not (parinfer--in-comment-or-string-p)))
       ;; Still, try not to run if we're in a string.
       ;;
       ;; The reason why comments are different is
       ;; because the starting point of a comment is in
       ;; the comment while the starting point of a
       ;; string is not in the string.
       (not (parinfer--in-string-p))
       (not (parinfer--unfinished-string-p))))

(defun parinfer--invoke-if-necessary ()
  "Invoke parinfer when necessary.

This is the entry point function added to `post-command-hook'."
  ;; Make sure `parinfer-region-mode' is synchronized with `use-region-p'.
  (cond ((and (not (bound-and-true-p parinfer-region-mode))
              (use-region-p))
         (parinfer-region-mode 1))
        ((and (bound-and-true-p parinfer-region-mode)
              (not (use-region-p)))
         (parinfer-region-mode -1)))
  (condition-case e
      (when (and this-command (symbolp this-command))
        (if (parinfer--should-clean-up-p)
            (parinfer--clean-up)
          (let ((parinfer--ppss (syntax-ppss)))
            (when (parinfer--should-invoke?)
              (cond
               ((parinfer-strategy-match-p this-command :instantly)
                (parinfer--invoke-immediately (point)))
               ((parinfer-strategy-match-p this-command :shift-right)
                (let ((parinfer--state 'indent))
                  (parinfer-readjust-paren))
                (save-excursion
                  (beginning-of-line)
                  (parinfer-readjust-indent)))
               ((parinfer-strategy-match-p this-command :default)
                (parinfer--invoke (point))
                (unless (parinfer--in-string-p)
                  (setq parinfer--text-modified t)))
               ;; Do nothing for everything else
               ;; (despite the name of the "default" strategy)
               (t nil))))))
    (error (message "Parinfer error: %s" e))))

(defun parinfer--active-line-region ()
  "Auto adjust region so that the shift can work properly."
  (setq parinfer--x-after-shift (- (point) (line-beginning-position)))
  (let* ((begin (region-beginning))
         (end (region-end))
         (new-begin (save-mark-and-excursion
                      (goto-char begin)
                      (line-beginning-position))))
    (goto-char new-begin)
    (set-mark-command nil)
    (goto-char end)
    (setq deactivate-mark nil)
    (setq parinfer--region-shifted t)))

(defun parinfer--shift (distance)
  "Shift text.  For right, DISTANCE > 0, for left, DISTANCE < 0."
  (when (use-region-p)
    (when (not parinfer--region-shifted)
      (parinfer--active-line-region))
    (let ((mark (mark)))
      (save-mark-and-excursion
        (indent-rigidly (region-beginning)
                        (region-end)
                        distance)
        (push-mark mark t t)
        (setq parinfer--x-after-shift
              (+ parinfer--x-after-shift distance))
        (setq deactivate-mark nil)))))

(defun parinfer-mode-enable ()
  "Enable `parinfer-mode'."
  (setq-mode-local parinfer-mode indent-tabs-mode nil)
  (setq parinfer--last-line-number (line-number-at-pos (point)))
  (add-hook 'post-command-hook 'parinfer--invoke-if-necessary t t)
  (parinfer--extension-lifecycle :mount)
  (parinfer--init)
  (run-hooks 'parinfer-mode-enable-hook))

(defun parinfer-mode-disable ()
  "Disable `parinfer-mode'."
  (remove-hook 'post-command-hook 'parinfer--invoke-if-necessary t)
  (parinfer--extension-lifecycle :unmount)
  (parinfer-region-mode -1)
  (run-hooks 'parinfer-mode-disable-hook))

(defun parinfer--auto-switch-to-indent-p ()
  "Should we automatically switch to indent state?"
  (and (not parinfer--first-load)
       (pcase parinfer-change-to-indent-state-automatically
         (`closing
          (let ((l-c-e last-command-event))
            (and (characterp l-c-e)
                 (string-match-p "\\s)"
                                 (string l-c-e)))))
         (_ parinfer-change-to-indent-state-automatically))
       (parinfer--paren-balanced-p)))

(cl-defun parinfer--apply-result (result &key (offset 1))
  "Apply changes in RESULT.

OFFSET specifies an offset between line numbers in the result and
real line numbers."
  (cl-loop for l in (plist-get result :changed-lines) do
           (parinfer--goto-line (+ offset (plist-get l :line-no)))
           (save-mark-and-excursion
             (delete-region (line-beginning-position)
                            (line-end-position)))
           (insert (plist-get l :line))))

(defun parinfer--handle-error (err &optional err-line)
  "Show the appropriate message for ERR.
ERR is an error object created by `parinferlib--create-error'.
ERR-LINE is where it happened. If nil, use ERR\\='s `:line-no' instead."
  (when parinfer-display-error
    (let ((err-line (or err-line (plist-get err :line-no))))
      (message "Parinfer error: %s at line/column %s:%s"
               (plist-get err :message)
               err-line
               (save-mark-and-excursion
                 (parinfer--goto-line err-line)
                 (forward-char (plist-get err :x))
                 (current-column))))))

(defun parinfer--readjust-paren-1 (text &optional options)
  "Wrapper around `parinferlib-indent-mode' that deals with tabs correctly.

TEXT and OPTIONS are passed to `parinferlib-indent-mode'."
  (let ((parinferlib--DOUBLE_SPACE (make-string tab-width ?\s)))
    (parinferlib-indent-mode text options)))

(defun parinfer-readjust-paren (&optional delay)
  "Readjust parens for indent state.

Readjust parens according to indentation.

When DELAY is non-nil and the text to be modified is larger than
`parinfer-delay-invoke-threshold', parens will only be modified
after `parinfer-delay-invoke-idle' seconds of idle time."
  (let* ((start (save-mark-and-excursion (parinfer--goto-previous-toplevel) (point)))
         (end (save-mark-and-excursion (parinfer--goto-next-toplevel) (point)))
         (text (buffer-substring-no-properties start end))
         (line-number (line-number-at-pos))
         (cursor-line (- line-number (line-number-at-pos start)))
         result err)
    ;; Don't touch the timer if we're not delayed.
    ;; I don't know if this is necessary.
    (when (and delay parinfer--delay-timer)
      (cancel-timer parinfer--delay-timer)
      (setq parinfer--delay-timer nil))
    (if (and delay (> (length text) parinfer-delay-invoke-threshold))
        (setq parinfer--delay-timer
              (run-with-idle-timer
               parinfer-delay-invoke-idle
               nil
               #'parinfer-readjust-paren))
      (unless (parinfer--unsafe-p)
        (setq result (parinfer--readjust-paren-1
                      text
                      (list :cursor-x (parinfer--cursor-x)
                            :cursor-line cursor-line
                            :preview-cursor-scope parinfer-preview-cursor-scope))
              err (plist-get result :error))
        (if err
            ;; HACK: sometimes TEXT can end up splitting up a string, even
            ;; though the string is properly closed. This would result in this
            ;; function failing even though `parinfer-readjust-paren-buffer'
            ;; succeeds. Just retry.
            (if (equal (plist-get err :name)
                       "unclosed-quote")
                (parinfer-readjust-paren-buffer)
              (parinfer--handle-error err (+ (line-number-at-pos start)
                                             (plist-get err :line-no))))
          (when (plist-get result :success)
            (parinfer--apply-result result :offset (line-number-at-pos start))
            (parinfer--goto-line line-number)
            (forward-char (plist-get result :cursor-x)))
          (setq parinfer--text-modified nil))
        (run-hooks 'parinfer-after-execute-hook)))))

(defun parinfer-readjust-paren-buffer ()
  "Readjust parens on the entire buffer."
  (let* ((window-start-pos (window-start))
         (cursor-line (1- (line-number-at-pos)))
         (cursor-x (parinfer--cursor-x))
         (opts (list :cursor-x cursor-x
                     :cursor-line cursor-line
                     :preview-cursor-scope parinfer-preview-cursor-scope))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (result (parinfer--readjust-paren-1 text opts))
         (err (plist-get result :error)))
    (if err
        (parinfer--handle-error err)
      (when (plist-get result :success)
        (parinfer--apply-result result)
        (parinfer--goto-line (1+ cursor-line))
        (forward-char (plist-get result :cursor-x))
        (set-window-start (selected-window) window-start-pos)))))

(defun parinfer--indent-changes ()
  "Does switching to indent state change the buffer?

Return `changed' if so, `unchanged' if not, or `(error <ERR>)' if
parinferlib returned an error."
  (let* ((input-text (buffer-substring-no-properties (point-min) (point-max)))
         (result (parinfer--readjust-paren-1
                  input-text
                  (list :cursor-line (1- (line-number-at-pos))
                        :cursor-x (parinfer--cursor-x)))))
    (cond ((not (plist-get result :success))
           (list 'error (plist-get result :error)))
          ((and (plist-get result :changed-lines)
                (not (string= input-text
                              (plist-get result :text))))
           'changed)
          (t 'unchanged))))

(parinfer--defcmd parinfer-readjust-paren-with-confirm ()
  "Call parinfer indent on whole buffer.

If there's any change, display a confirm message in minibuffer."
  (let* ((window-start-pos (window-start))
         (orig-cursor-line (line-number-at-pos))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (result (parinfer--readjust-paren-1
                  text
                  (list :cursor-line (1- orig-cursor-line)
                        :cursor-x (parinfer--cursor-x))))
         (err (plist-get result :error))
         (error-message (plist-get err :message))
         (error-line-no (plist-get err :line-no)))
    (cond
     ((not (plist-get result :success))
      (prog1 nil
        (parinfer--errmsg error-line-no error-message)))
     ((and (not (plist-get result :changed-lines))
           (string= text (plist-get result :text)))
      t)
     ((y-or-n-p "Parinfer: Switching to Indent will modify this buffer, continue? ")
      (progn
        (parinfer--apply-result result)
        (parinfer--goto-line orig-cursor-line)
        (forward-char (plist-get result :cursor-x))
        (set-window-start (selected-window) window-start-pos)
        t)))))

(defun parinfer-readjust-indent ()
  "Readjust indentation for paren state.

This relies on Emacs's own indentation facilities instead of
Parinfer's algorithm in order to correctly indent according to
major mode rules."
  (let (result)
    (setq result (ignore-errors (parinfer--reindent-sexp)))
    (when (and result
               (parinfer--auto-switch-to-indent-p))
      (parinfer--change-to-state 'indent))))

;;;; Commands

(parinfer--defcmd parinfer-auto-fix ()
  "Reindent the entire buffer then readjust parens."
  (untabify (point-min) (point-max))
  (dolist (cmd '(mark-whole-buffer
                 indent-region
                 keyboard-quit))
    (call-interactively cmd))
  (parinfer-readjust-paren-buffer))

(parinfer--defcmd parinfer-ediff-quit ()
  "Quit `parinfer-diff' directly, without confirm."
  (ediff-really-quit nil)
  (with-current-buffer "*Parinfer Result*"
    (kill-buffer-and-window)))

(parinfer--defcmd parinfer-backward-delete-char ()
  "Replacement in command ‘parinfer-mode’ for ‘backward-delete-char’ command."
  (if (eq 'paren parinfer--state)
      (parinfer-run
       (if (string-match-p "^[[:space:]]+$"
                           (buffer-substring-no-properties
                            (line-beginning-position)
                            (point)))
           (delete-indentation)
         (backward-delete-char 1)))
    (progn
      (backward-delete-char 1)
      (if (parinfer--in-string-p)
          (setq parinfer--text-modified nil)
        (parinfer--invoke)))))

(parinfer--defcmd parinfer-backward-kill-word ()
  "Replacement in symbol `parinfer-mode' for `backward-kill-word' command."
  (parinfer-run
   (call-interactively 'backward-kill-word)))

(parinfer--defcmd parinfer-delete-char ()
  "Replacement in `parinfer-mode' for `delete-char' command."
  (parinfer-run
   (delete-char 1)))

(parinfer--defcmd parinfer-kill-word ()
  "Replacement in `parinfer-mode' for `kill-word' command."
  (parinfer-run
   (call-interactively 'kill-word)))

(parinfer--defcmd parinfer-kill-line ()
  "Replacement in `parinfer-mode' for `kill-line' command."
  (parinfer-run
   (call-interactively 'kill-line)))

(parinfer--defcmd parinfer-delete-indentation ()
  "Replacement in `parinfer-mode' for `delete-indentation' command."
  (parinfer-paren-run
   (call-interactively 'delete-indentation)))

(parinfer--defcmd parinfer-raise-sexp ()
  "Raise sexp and Indent code."
  (call-interactively 'raise-sexp)
  (parinfer--reindent-sexp))

(parinfer--defcmd parinfer-region-delete-region ()
  "Delete region then invoke parinfer afterwards."
  (parinfer-run
   (call-interactively 'delete-region)
   (deactivate-mark t)))

(parinfer--defcmd parinfer-yank ()
  "`yank', then reindent the buffer."
  (call-interactively 'yank)
  (setq parinfer--text-modified t)
  (parinfer-readjust-paren-buffer))

(parinfer--defcmd parinfer-mouse-drag-region ()
  "Should do clean up if it is needed."
  (when parinfer--delay-timer
    (parinfer--clean-up))
  (call-interactively 'mouse-drag-region))

(parinfer--defcmd parinfer-kill-region ()
  "Replacement in `parinfer-mode' for `kill-region' command."
  (parinfer-run
   (call-interactively 'kill-region)))

(parinfer--defcmd parinfer-newline ()
  "Replacement in `parinfer-mode' for `newline' command."
  (parinfer-do
   (call-interactively 'newline)))

(parinfer--defcmd parinfer-semicolon ()
  "Insert semicolon, always indent after insertion.

This is the very special situation, since we always need
invoke parinfer after every semicolon input."
  (call-interactively 'self-insert-command)
  (parinfer-readjust-paren t)
  (setq parinfer--text-modified t))

(parinfer--defcmd parinfer-double-quote ()
  "Insert a pair of quotes, or a single quote after backslash."
  (cond
   ;; insert just one quote after backslash
   ((= (char-before) ?\\)
    (insert "\""))
   ;; Insert \"\" in a string
   ((parinfer--in-string-p)
    ;; Skip through the quote if we're at the end of a non-empty string
    (if (and (= (char-after) ?\")
             (not (= (char-before) ?\")))
        (forward-char 1)
      (insert "\\\"\\\"")
      (forward-char -2)))
   ;; Otherwise insert a pair of quotes
   (t
    (insert "\"\"")
    (parinfer--invoke-if-necessary)
    ;; Manage the whitespace
    (unless (or (eolp)
                (eql #x29 (char-after (point)))
                (eql #x20 (char-after (point))))
      (insert " ")
      (forward-char -1))
    (forward-char -1))))

(parinfer--defcmd parinfer-toggle-state ()
  "Toggle Parinfer between indent state and paren state."
  (cond ((eq 'indent parinfer--state)
         (parinfer--change-to-state 'paren))
        ((or (not parinfer--first-load)
             ;; Don't readjust in temporary buffers.
             ;; - temporary buffer = name starts with a space
             ;; - take advantage of how buffer name cannot be empty
             ;; - this is from `yas-temp-buffer-p'
             (eq ?\s (aref (buffer-name) 0)))
         (parinfer-readjust-paren-buffer)
         (parinfer--change-to-state 'indent))
        (t
         (when (parinfer-readjust-paren-with-confirm)
           (parinfer--change-to-state 'indent)))))

(parinfer--defcmd parinfer-diff ()
  "Diff current code and the code after readjusting parens in Ediff.
Use this to browse and apply the changes."
  (let* ((orig-text (buffer-substring-no-properties (point-min) (point-max)))
         (new-buffer (generate-new-buffer "*Parinfer Result*"))
         (orig-buffer (current-buffer))
         (m major-mode)
         (result (parinfer--readjust-paren-1 orig-text nil)))
    (with-current-buffer new-buffer
      (erase-buffer)
      (insert (plist-get result :text))
      (funcall m)
      (ediff-buffers orig-buffer new-buffer '(parinfer--ediff-init-keys)))))

(parinfer--defcmd parinfer-region-mode-toggle-state ()
  "Show a user error.

The state cannot be changed while the region is active."
  (user-error "Parinfer: Cannot change state when region is active"))

(parinfer--defcmd parinfer-shift-right ()
  "In indent state with region active, shift text left."
  (when (eq 'indent parinfer--state)
    (parinfer--shift 1)))

(parinfer--defcmd parinfer-shift-left ()
  "In indent state with region active, shift text left."
  (when (eq 'indent parinfer--state)
    (parinfer--shift -1)))

;;;; Keymaps

(defvar parinfer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap backward-delete-char-untabify] 'parinfer-backward-delete-char)
    (define-key map [remap delete-backward-char] 'parinfer-backward-delete-char)
    (define-key map [remap mouse-drag-region] 'parinfer-mouse-drag-region)
    (define-key map [remap delete-indentation] 'parinfer-delete-indentation)
    (define-key map ";" 'parinfer-semicolon)
    (define-key map "\"" 'parinfer-double-quote)
    (define-key map [remap yank] 'parinfer-yank)
    map))

(defvar parinfer-region-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>")       #'parinfer-shift-right)
    (define-key map (kbd "S-<tab>")     #'parinfer-shift-left)
    (define-key map (kbd "TAB")         #'parinfer-shift-right)
    (define-key map (kbd "<backtab>")   #'parinfer-shift-left)
    (define-key map (kbd "<backspace>") #'parinfer-region-delete-region)
    (define-key map [remap parinfer-toggle-state]
      #'parinfer-region-mode-toggle-state)
    map))

;;;; Minor mode

;;;###autoload
(define-minor-mode parinfer-mode
  "Parinfer mode."
  :init-value nil
  :lighter (:eval (parinfer--lighter))
  :keymap parinfer-mode-map
  (if parinfer-mode
      (parinfer-mode-enable)
    (parinfer-mode-disable)))

;;;###autoload
(define-minor-mode parinfer-region-mode
  "Whether region is active in parinfer-mode.

Used to provide a keymap while region is active, and for running
some code after region is deactivated. Do not manually enable
this mode."
  :init-value nil
  :keymap parinfer-region-mode-map
  (if parinfer-region-mode
      ;; Entering: do nothing.
      nil
    ;; Exiting: readjust paren if needed
    (when (and (eq 'indent parinfer--state)
               parinfer--region-shifted)
      (beginning-of-line)
      (parinfer-readjust-paren)
      (when parinfer--x-after-shift
        (if (> parinfer--x-after-shift
               (- (line-end-position) (line-beginning-position)))
            (end-of-line)
          (when (> parinfer--x-after-shift 0)
            (forward-char parinfer--x-after-shift))))
      (setq parinfer--region-shifted nil)
      (setq parinfer--x-after-shift nil))))

;;;; Debug mode
(defvar parinfer-debug nil)
(defun parinfer--debug (fmt &rest args)
  "Send a message in debug mode.
FMT and ARGS are like `message'."
  (when parinfer-debug
    (apply #'message fmt args)))
(cl-defmacro parinfer--add-debug-advice (symbol &key expected)
  "Add a debug advice on SYMBOL.
The advice sends a debug message if its return value is not
`equal' to EXPECTED."
  (let ((advice-name (intern (format "parinfer---advice-%s" symbol))))
    `(prog1 (defun ,advice-name
                (func &rest args)
              ,(format "Advice as added by `parinfer--add-debug-advice.'
Calls FUNC with ARGS and returns it; a debug message is sent if
the value is not %S."
                       expected)
              (let ((result (apply func args)))
                (prog1 result
                  (unless (equal result ,expected)
                    (parinfer--debug "%s: %s (%s)"
                                     this-command
                                     result
                                     ',symbol)))))
       (advice-add ',symbol :around ',advice-name))))
(parinfer--add-debug-advice parinfer--unsafe-p :expected nil)
(parinfer--add-debug-advice parinfer--should-invoke? :expected t)

(provide 'parinfer)
;;; parinfer.el ends here
