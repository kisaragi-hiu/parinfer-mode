;;; parinfer.el --- Simpler Lisp editing  -*- lexical-binding: t; -*-

;; Copyright (c) 2016, Shi Tianshu

;; Author: Shi Tianshu
;; Homepage: https://github.com/DogLooksGood/parinfer-mode
;; Version: kisaragi-0.5.0
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
(require 'parinfer-ext)
(require 'parinfer-strategies)

;;;; User options

(defgroup parinfer nil
  "Parinfer."
  :group 'lisp)

(defgroup parinfer-faces nil
  "Parinfer faces."
  :group 'faces)

(defcustom parinfer-auto-switch-indent-mode nil
  "Switch back to indent mode automatically if parens are balanced.

nil: never
t: after every command if parens are balanced
`closing': only after inserting a closing paren"
  :group 'parinfer
  :type '(choice (const :tag "Never" nil)
                 (const :tag "Whenever parens are balanced" t)
                 (const :tag "Only after inserting a close paren" closing)))

(defcustom parinfer-lighters
  '("Parinfer:Indent" . "Parinfer:Paren")
  "Parinfer lighters in mode line.

The modeline shows the car in indent mode and the cdr in paren
mode."
  :group 'parinfer
  :type '(cons (string :tag "Indent mode lighter")
               (string :tag "Paren mode lighter")))

(defcustom parinfer-preview-cursor-scope nil
  "Set it to t will show cursor scop in Indent Mode.

It will show the cursor's scope on an empty line by inserting
close-parens after it."
  :group 'parinfer
  :type 'boolean)

(defcustom parinfer-delay-invoke-threshold 6000
  "Threshold for 'parinfer-mode' delay processing."
  :group 'parinfer
  :type 'number)

(defcustom parinfer-delay-invoke-idle 0.3
  "The delay time(seconds) for parinfer delay processing."
  :group 'parinfer
  :type 'number)

(defcustom parinfer-display-error nil
  "If display error when parinfer failed in Indent Mode."
  :group 'parinfer
  :type 'boolean)

(defvar parinfer-mode-enable-hook nil
  "Call after parinfer mode is enabled.")

(defvar parinfer-mode-disable-hook nil
  "Call after parinfer mode is disabled.")

(defvar parinfer-switch-mode-hook nil
  "Call after parinfer mode switch between Indent Mode & Paren Mode.

One argument for hook function, MODE present for the mode will be used.")

(defvar parinfer-after-execute-hook nil
  "Call after parinfer executed.")

;;;; Internal variables and constants

(defvar-local parinfer--mode 'paren
  "Parinfer mode style, 'paren or 'indent.")

(defvar-local parinfer--first-load t
  "If the buffer haven't switch to indent mode yet.")

(defvar-local parinfer--region-shifted nil
  "If shift the region after mark activate.")

(defvar-local parinfer--text-modified nil
  "If last command modified text.")

(defvar-local parinfer--last-line-number -1
  "Holds the last line number after invoke-parinfer-when-necessary.")

(defvar-local parinfer--delay-timer nil
  "Current delay timer.")

(defvar-local parinfer--x-after-shift nil
  "Where the cursor x should be, after shift region.")

;;;; Helper macros

(defmacro parinfer-silent (&rest body)
  "Run BODY with `message' silenced."
  `(cl-letf (((symbol-function 'message) #'format))
     ,@body))

(defmacro parinfer-paren-run (&rest body)
  "Run BODY in paren mode.  Keep S-sexp in correct indentation."
  (let ((toggle (make-symbol "toggle")))
    `(let ((,toggle (eq parinfer--mode 'indent)))
       (parinfer-silent
        (when ,toggle
          (parinfer--switch-to-paren-mode))
        ,@body
        (when ,toggle
          (parinfer--reindent-sexp)
          (parinfer--indent-and-switch-to-indent-mode))))))

(defmacro parinfer-run (&rest body)
  "Run BODY, then invoke parinfer.

Reset delay if exists."
  `(progn
     ,@body
     (setq parinfer--text-modified t)
     (parinfer--invoke-parinfer)))

(defmacro parinfer-do (&rest body)
  "Run BODY, then invoke parinfer.

Clean up delay if exists."
  `(progn
     (when parinfer--delay-timer
       (parinfer--clean-up))
     ,@body
     (setq parinfer--text-modified t)
     (parinfer--invoke-parinfer)))

;;;; Helper functions

(defun parinfer--reindent-sexp ()
  "Reindent current sexp."
  (interactive)
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
       (format "Parinfer: Error on line %d: %s. Switching to Paren mode."
               line-number msg)
     (format "Parinfer: Error: %s. Switching to Paren mode."
             msg))))

;;;; Minor mode logic

(defun parinfer--init ()
  "Init Parinfer Mode, switch to Paren firstly, then Indent."
  (parinfer--switch-to-paren-mode)
  (pcase (parinfer--indent-changes)
    (`unchanged
     (parinfer--switch-to-indent-mode))
    (`changed
     (message
      (substitute-command-keys
       (concat "Parinfer: Paren mode, use \\[parinfer-toggle-mode] "
               "to switch to Indent mode."))))
    (`(error ,err)
     (parinfer--errmsg
      (plist-get err :line-no)
      (plist-get err :message)))))

(defun parinfer--indent-and-switch-to-indent-mode ()
  "Switch to Indent mode and call Indent Mode immediately."
  (interactive)
  (parinfer--switch-to-indent-mode)
  (parinfer--invoke-parinfer-when-necessary))

(defun parinfer--switch-to-indent-mode ()
  "Switch to indent mode."
  (setq parinfer--mode 'indent)
  (setq parinfer--first-load nil)
  (run-hook-with-args 'parinfer-switch-mode-hook 'indent)
  (parinfer--extension-lifecycle :indent)
  (force-mode-line-update))

(defun parinfer--switch-to-paren-mode ()
  "Switch to paren mode."
  (when parinfer--delay-timer
    (parinfer--clean-up))
  (setq parinfer--mode 'paren)
  (run-hook-with-args 'parinfer-switch-mode-hook 'paren)
  (parinfer--extension-lifecycle :paren)
  (force-mode-line-update))

(defun parinfer--in-comment-or-string-p ()
  "Return if we are in comment or string."
  (let ((f (get-text-property (point) 'face))
        (ppss (syntax-ppss)))
    (or (nth 3 ppss)
        (nth 4 ppss)
        (eq f 'font-lock-comment-face)
        (eq f 'font-lock-comment-delimiter-face))))

(defun parinfer--in-string-p ()
  "Return if we are in string."
  (nth 3 (syntax-ppss)))

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
                (or (parinfer--in-comment-or-string-p)
                    (parinfer--empty-line-p)
                    (not (eq (point) (line-beginning-position)))
                    (not (parinfer--toplevel-line-p))))
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
              (if (and (not (or (parinfer--in-comment-or-string-p)
                                (parinfer--empty-line-p)))
                       (eq (point) (line-beginning-position))
                       (parinfer--toplevel-line-p))
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
  "Return the lighter for the current mode."
  (let ((str (if (eq 'paren parinfer--mode)
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
  "Get the cursor-x which is need by parinferlib computation."
  (abs (- (line-beginning-position) (point))))

(defun parinfer--invoke-parinfer-instantly (&optional pos)
  "Invoke Parinfer immediately.

Readjust indentation in paren mode, readjust parens in indent
mode. Paren readjustment is performed immediately, regardless of
`parinfer-delay-invoke-threshold'.

POS is the position we want to call parinfer."
  (if (and pos (not (eq pos (point))))
      (let ((ln (line-number-at-pos))
            (x (parinfer--cursor-x)))
        (goto-char pos)
        (parinfer--invoke-parinfer-instantly)
        (parinfer--goto-line ln)
        (forward-char x))
    (cond
     ((eq 'paren parinfer--mode) (parinfer-readjust-indent))
     ((eq 'indent parinfer--mode) (parinfer-readjust-paren))
     (t "nothing"))))

(defun parinfer--invoke-parinfer (&optional pos)
  "Invoke Parinfer.

Readjust indentation in paren mode, readjust parens in indent
mode. Paren readjustment is delayed after
`parinfer-delay-invoke-idle' seconds of idle time if the text to
be changed is longer than `parinfer-delay-invoke-threshold'.

POS is the position we want to call parinfer."
  (if (and pos (not (eq pos (point))))
      (let ((current-pos (point)))
        (goto-char pos)
        (parinfer--invoke-parinfer)
        (goto-char current-pos))
    (cond
     ((eq 'paren parinfer--mode) (parinfer-readjust-indent))
     ((eq 'indent parinfer--mode) (parinfer-readjust-paren t))
     (t "nothing"))))

(defun parinfer--should-clean-up-p ()
  "Should parinfer do clean job."
  (and (eq parinfer--mode 'indent)
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
    (parinfer--invoke-parinfer-instantly pos)))

(defun parinfer--comment-line-p ()
  "Return whether the current line is a comment."
  (save-mark-and-excursion
    (back-to-indentation)
    (let ((f (get-text-property (point) 'face)))
      (and (string-match-p "^;+.*$" (buffer-substring-no-properties (point) (line-end-position)))
           (save-mark-and-excursion
             (end-of-line)
             (or (nth 4 (syntax-ppss))
                 (eq f 'font-lock-comment-face)
                 (eq f 'font-lock-comment-delimiter-face)))))))

(defun parinfer--invoke-parinfer-when-necessary ()
  "Invoke parinfer when necessary.

This is the entry point function added to `post-command-hook'."
  ;; Make sure `parinfer-region-mode' is synchronized with `use-region-p'.
  (cond ((and (not (bound-and-true-p parinfer-region-mode))
              (use-region-p))
         (parinfer--region-mode-enable))
        ((and (bound-and-true-p parinfer-region-mode)
              (not (use-region-p)))
         (parinfer--region-mode-disable)))
  (when (and this-command (symbolp this-command))
    (if (parinfer--should-clean-up-p)
        (parinfer--clean-up)
      (unless (or (parinfer-strategy-match-p this-command :skip)
                  (parinfer--in-comment-or-string-p)
                  (parinfer--unfinished-string-p))
        (cond
         ((parinfer-strategy-match-p this-command :instantly)
          (parinfer--invoke-parinfer-instantly (point)))
         ((parinfer-strategy-match-p this-command :shift-right)
          (let ((parinfer--mode 'indent))
            (parinfer-readjust-paren))
          (save-excursion
            (beginning-of-line)
            (parinfer-readjust-indent)))
         ((parinfer-strategy-match-p this-command :default)
          (parinfer--invoke-parinfer (point))
          (unless (parinfer--in-string-p)
            (setq parinfer--text-modified t))))))))

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
  "Enable 'parinfer-mode'."
  (setq-mode-local parinfer-mode indent-tabs-mode nil)
  (setq parinfer--last-line-number (line-number-at-pos (point)))
  (add-hook 'post-command-hook 'parinfer--invoke-parinfer-when-necessary t t)
  (parinfer--extension-lifecycle :mount)
  (parinfer--init)
  (run-hooks 'parinfer-mode-enable-hook))

(defun parinfer-mode-disable ()
  "Disable 'parinfer-mode'."
  (remove-hook 'post-command-hook 'parinfer--invoke-parinfer-when-necessary t)
  (parinfer--extension-lifecycle :unmount)
  (parinfer--region-mode-disable)
  (run-hooks 'parinfer-mode-disable-hook))

(defun parinfer--region-mode-enable ()
  "Run when region activated."
  (parinfer-region-mode 1))

(defun parinfer--region-mode-disable ()
  "Run when region deactivated, indent code if ‘parinfer--mode’ is 'indent."
  (when (and (eq 'indent parinfer--mode)
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
    (setq parinfer--x-after-shift nil))
  (parinfer-region-mode -1))

(defun parinfer--auto-switch-indent-mode-p ()
  "Should we automatically switch to indent mode?"
  (and (parinfer--paren-balanced-p)
       (not parinfer--first-load)
       (pcase parinfer-auto-switch-indent-mode
         (`closing
          (let ((l-c-e last-command-event))
            (and (characterp l-c-e)
                 (string-match-p "\\s)"
                                 (string l-c-e)))))
         (_ parinfer-auto-switch-indent-mode))))

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

(defun parinfer--readjust-paren-1 (text &optional options)
  "Wrapper around `parinferlib-indent-mode' that deals with tabs correctly.

TEXT and OPTIONS are passed to `parinferlib-indent-mode'."
  (let ((parinferlib--DOUBLE_SPACE (make-string tab-width ?\s)))
    (parinferlib-indent-mode text options)))

(defun parinfer-readjust-paren (&optional delay)
  "Parinfer indent mode.

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
        (if (and parinfer-display-error err)
            (let ((err-line (+ (line-number-at-pos start)
                               (plist-get err :line-no))))
              (message "Parinfer error:%s at line: %s column:%s"
                       (plist-get err :message)
                       err-line
                       (save-mark-and-excursion
                         (parinfer--goto-line err-line)
                         (forward-char (plist-get err :x))
                         (current-column))))
          (when (plist-get result :success)
            (parinfer--apply-result result :offset (line-number-at-pos start))
            (parinfer--goto-line line-number)
            (forward-char (plist-get result :cursor-x)))
          (setq parinfer--text-modified nil))
        (run-hooks 'parinfer-after-execute-hook)))))

(defun parinfer-readjust-paren-buffer ()
  "Call parinfer indent on whole buffer."
  (let* ((window-start-pos (window-start))
         (cursor-line (1- (line-number-at-pos)))
         (cursor-x (parinfer--cursor-x))
         (opts (list :cursor-x cursor-x
                     :cursor-line cursor-line
                     :preview-cursor-scope parinfer-preview-cursor-scope))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (result (parinfer--readjust-paren-1 text opts)))
    (when (plist-get result :success)
      (parinfer--apply-result result)
      (parinfer--goto-line (1+ cursor-line))
      (forward-char (plist-get result :cursor-x))
      (set-window-start (selected-window) window-start-pos))))

(defun parinfer--indent-changes ()
  "Does switching to indent mode change the buffer?

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

(defun parinfer-readjust-paren-with-confirm ()
  "Call parinfer indent on whole buffer.

If there's any change, display a confirm message in minibuffer."
  (interactive)
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
  "Readjust indentation for paren mode.

This relies on Emacs's own indentation facilities instead of
Parinfer's algorithm in order to correctly indent according to
major mode rules."
  (let (result)
    (setq result (ignore-errors (parinfer--reindent-sexp)))
    (when (and result
               (parinfer--auto-switch-indent-mode-p))
      (parinfer--switch-to-indent-mode))))

;;;; Commands

(defun parinfer-untabify-buffer ()
  "Untabify whole buffer.

Currently parinfer can not handle indentation with tab.
Use this to remove tab indentation of your code."
  (interactive)
  (untabify (point-min) (point-max)))

(defun parinfer-auto-fix ()
  "Untabify whole buffer then reindent whole buffer."
  (interactive)
  (parinfer-untabify-buffer)
  (dolist (cmd '(mark-whole-buffer
                 indent-region
                 keyboard-quit))
    (call-interactively cmd))
  (parinfer-readjust-paren-buffer))

(defun parinfer-ediff-quit ()
  "Quit ‘parinfer-diff’ directly, without confirm."
  (interactive)
  (ediff-really-quit nil)
  (with-current-buffer "*Parinfer Result*"
    (kill-buffer-and-window)))

(defun parinfer-backward-delete-char ()
  "Replacement in command ‘parinfer-mode’ for ‘backward-delete-char’ command."
  (interactive)
  (if (eq 'paren parinfer--mode)
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
        (parinfer--invoke-parinfer)))))

(defun parinfer-backward-kill-word ()
  "Replacement in symbol 'parinfer-mode' for 'backward-kill-word' command."
  (interactive)
  (parinfer-run
   (call-interactively 'backward-kill-word)))

(defun parinfer-delete-char ()
  "Replacement in 'parinfer-mode' for 'delete-char' command."
  (interactive)
  (parinfer-run
   (delete-char 1)))

(defun parinfer-kill-word ()
  "Replacement in 'parinfer-mode' for 'kill-word' command."
  (interactive)
  (parinfer-run
   (call-interactively 'kill-word)))

(defun parinfer-kill-line ()
  "Replacement in 'parinfer-mode' for 'kill-line' command."
  (interactive)
  (parinfer-run
   (call-interactively 'kill-line)))

(defun parinfer-delete-indentation ()
  "Replacement in 'parinfer-mode' for 'delete-indentation' command."
  (interactive)
  (parinfer-paren-run
   (call-interactively 'delete-indentation)))

(defun parinfer-raise-sexp ()
  "Raise sexp and Indent code."
  (interactive)
  (call-interactively 'raise-sexp)
  (parinfer--reindent-sexp))

(defun parinfer-region-delete-region ()
  "Delete region if active, backspace if not (?)."
  (interactive)
  (if (region-active-p)
      (call-interactively 'delete-region)
    (call-interactively 'parinfer-backward-delete-char))
  (deactivate-mark t)
  (parinfer-run))

(defun parinfer-yank ()
  "`yank', then reindent the buffer."
  (interactive)
  (call-interactively 'yank)
  (setq parinfer--text-modified t)
  (parinfer-readjust-paren-buffer))

(defun parinfer-mouse-drag-region ()
  "Should do clean up if it is needed."
  (interactive)
  (when parinfer--delay-timer
    (parinfer--clean-up))
  (call-interactively 'mouse-drag-region))

(defun parinfer-kill-region ()
  "Replacement in 'parinfer-mode' for 'kill-region' command."
  (interactive)
  (parinfer-run
   (call-interactively 'kill-region)))

(defun parinfer-newline ()
  "Replacement in 'parinfer-mode' for 'newline' command."
  (interactive)
  (parinfer-do
   (call-interactively 'newline)))

(defun parinfer-semicolon ()
  "Insert semicolon, always indent after insertion.

This is the very special situation, since we always need
invoke parinfer after every semicolon input."
  (interactive)
  (call-interactively 'self-insert-command)
  (parinfer-readjust-paren t)
  (setq parinfer--text-modified t))

(defun parinfer-double-quote ()
  "Insert a pair of quotes, or a single quote after backslash."
  (interactive)
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
    (parinfer--invoke-parinfer-when-necessary)
    ;; Manage the whitespace
    (unless (or (eolp)
                (eql #x29 (char-after (point)))
                (eql #x20 (char-after (point))))
      (insert " ")
      (forward-char -1))
    (forward-char -1))))

(defun parinfer-toggle-mode ()
  "Switch parinfer mode between Indent Mode and Paren Mode."
  (interactive)
  (cond ((eq 'indent parinfer--mode)
         (parinfer--switch-to-paren-mode))
        ((or (not parinfer--first-load)
             (string= (buffer-name) " *temp*"))
         (parinfer-readjust-paren-buffer)
         (parinfer--switch-to-indent-mode))
        (t
         (when (parinfer-readjust-paren-with-confirm)
           (parinfer--switch-to-indent-mode)))))

(defun parinfer-diff ()
  "Diff current code and the code after applying Indent Mode in Ediff.
Use this to browse and apply the changes."
  (interactive)
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

(defun parinfer-region-mode-switch-mode ()
  "While when 'parinfer-region-mode’ is enabled, we can't switch to Paren Mode."
  (interactive)
  (message "Can't toggle Parinfer Mode when region is active."))

(defun parinfer-shift-right ()
  "In Indent Mode with region active, shift text left."
  (interactive)
  (when (eq 'indent parinfer--mode)
    (parinfer--shift 1)))

(defun parinfer-shift-left ()
  "In Indent Mode with region active, shift text left."
  (interactive)
  (when (eq 'indent parinfer--mode)
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
    (define-key map (kbd "<tab>") 'parinfer-shift-right)
    (define-key map (kbd "S-<tab>") 'parinfer-shift-left)
    (define-key map (kbd "TAB") 'parinfer-shift-right)
    (define-key map (kbd "<backtab>") 'parinfer-shift-left)
    (define-key map (kbd "<backspace>") 'parinfer-region-delete-region)
    (define-key map [remap parinfer-toggle-mode] 'parinfer-region-mode-switch-mode)
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
  "Available when region is active."
  :init-value nil
  :keymap parinfer-region-mode-map)

(provide 'parinfer)
;;; parinfer.el ends here
