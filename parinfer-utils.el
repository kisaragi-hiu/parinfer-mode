;;; parinfer-utils.el --- Utilities -*- lexical-binding: t -*-

;;; Commentary:

;; commentary

;;; Code:

(defmacro parinfer--defcmd
    (name arglist &optional docstring &rest body)
  "Define a Parinfer command called NAME.
NAME, ARGLIST, DOCSTRING, and BODY are passed to `defun'.

The defined command is marked as belonging to `parinfer-mode'. An
`interactive' form can still be used as the first element of
BODY; the spec is passed along, but its MODES would be
discarded."
  (declare (doc-string 3) (indent 2))
  (let ((before nil)
        (spec nil))
    ;; Handle docstring, declaration, and body arguments
    (progn
      ;; ("docstring")
      (when (and (stringp docstring)
                 (not body))
        (setq body (list docstring)
              docstring nil))
      ;; ((declare) ,@body)
      ;; ("docstring" ,@body)
      (when (or (stringp docstring)
                (eq 'declare (car-safe docstring)))
        (setq before (list docstring)))
      ;; ("docstring" (declare) ,@rest)
      (when (and (stringp docstring)
                 (eq 'declare (car-safe (car body))))
        (setq before (list docstring (car body))
              body (cdr body)))
      ;; (,@body)
      (unless before
        (setq body (cons docstring body))))
    ;; Passing in spec with an extra interactive form
    (when (eq 'interactive (car-safe (car body)))
      (setq spec (elt (car body) 1))
      (setq body (cdr body)))
    `(defun ,name ,arglist
       ,@before
       ,@(if (version< emacs-version "28")
             `((interactive))
           `((interactive ,spec parinfer-mode)))
       ,@body)))

(defun parinfer--plist2alist (plist)
  "Convert a property PLIST to an association list."
  (let (key output)
    (dolist (x plist)
      (if (keywordp x)
          (progn (setq key x)
                 (push (list key) output))
        (push `(,@(assq key output) ,x) output)))
    output))

(provide 'parinfer-utils)

;;; parinfer-utils.el ends here
