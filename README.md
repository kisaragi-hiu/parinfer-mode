# Why fork

[Parinfer Mode](https://github.com/DogLooksGood/parinfer-mode) is unmaintained.

[parinfer-rust-mode](https://github.com/justinbarclay/parinfer-rust-mode) seems okay if it works, but it...

- requires either having a Rust compiler or downloading a prebuilt binary
- relies on a [fork](https://github.com/justinbarclay/parinfer-rust) of [parinfer-rust](https://github.com/eraserhd/parinfer-rust), and
- has itself not had an update since April 2021.

As I rely on this mode to do all my Lisp editing, I figured I might as well make some changes to my local copy.

# Main differences

- Load time for each .el file decreased by at least 50% as the paren readjustment algorithm was being run twice before
- evil integration (triggering paren or indent readjustment after the right commands) now available out of the box
- parinferlib 1.1.0 (upstreamed by me), which provides 50% better performance on parinferlib's test file
  - This comes from lexical binding, and using global variables for global state instead of passing a hash table around
- Tabs don't break buffers any more, instead they are just replaced with spaces
- Dropped Emacs 24 support
- Dropped experimental “One” mode

# What Is Parinfer

> ***Parinfer*** is a proof-of-concept editor mode for Lisp programming languages. It simplifies the way we write Lisp by auto-adjusting parens when indentation changes and vice versa. The hope is to make basic Lisp-editing easier for newcomers and experts alike, while still allowing existing plugins like Paredit to satisfy the need for more advanced operations.
>
> — <https://shaunlebron.github.io/parinfer/>

There are two modes (which I refer to as “states”), *Indent* and *Paren*. Indent state allows you to edit indentation, while Parinfer automatically adjusts parens; Paren state allows you to edit parens, while Parinfer automatically adjusts indentation.

# Installation

```elisp
(straight-use-package '(parinfer :host github :repo "kisaragi-hiu/parinfer-mode"))
```

Recommended configuration:

```elisp
(use-package parinfer-mode
  :init
  (setq parinfer-extensions
        (list 'defaults       ; should be included.
              'pretty-parens  ; use paren style to highlight which state we're in.
              'lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
              'paredit        ; Bindings for Paredit commands
              'smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
              'smart-yank))   ; state-dependent yank behavior.
  (add-hook 'clojure-mode-hook #'parinfer-mode)
  (add-hook 'scheme-mode-hook #'parinfer-mode)
  ;; Major modes run hooks of their entire ancestry. Adding this to
  ;; lisp-data-mode is enough for it to take effect in lisp-mode and
  ;; emacs-lisp-mode, both of which are derived from lisp-data-mode.
  (if (boundp 'lisp-data-mode-hook)
      (add-hook 'lisp-data-mode-hook #'parinfer-mode)
    ;; Before Emacs 28, emacs-lisp-mode derives from lisp-mode, so add
    ;; the hook there.
    (add-hook 'lisp-mode-hook #'parinfer-mode))
  :config
  (define-key parinfer-mode-map
    (kbd "C-,") #'parinfer-toggle-state))
```

# Commands

- `parinfer-toggle-state`:   toggle between indent state and paren state
- `parinfer-diff`:   show how switching to indent state will modify the buffer
- `parinfer-auto-fix`:   manual trigger for auto adjustment of indentation for the whole buffer (when parens are balanced)

# Configuration

- `parinfer-change-to-indent-state-automatically`{.verbatim}

    Possible values: `nil`{.verbatim} (default), `t`{.verbatim}, `closing`{.verbatim}

    Switch to indent state whenever parens are balanced in paren state.

    Set this to `closing`{.verbatim} to only do so after inserting a closing paren.

- `parinfer-delay-invoke-threshold`{.verbatim}

    Possible values: number of characters; defaults to 6000

    Text processing is normally done immediately after a command completes. If the text to be processed has more than this number of characters, it will instead be done in an idle timer (after `parinfer-delay-invoke-idle`{.verbatim} seconds, which defaults to 0.3).

- `parinfer-lighters`{.verbatim} (default: `("Parinfer:Indent" . "Parinfer:Paren")`{.verbatim})

    The mode line shows "Parinfer:Indent" when Parinfer is active in indent state, "Parinfer:Paren" if it is in paren state. The car is the indent state string, while the cdr is the paren state string.

    There is no need to add the leading space. It is added automatically.

- `parinfer-extensions`{.verbatim} (default: `(defaults pretty-parens smart-yank)`{.verbatim})

    Enabled extensions.

    An extension contains different pieces of code that run at different stages (entering indent state, entering paren state, enabling and disabling `parinfer-mode`{.verbatim}).

    Possible extensions:

| Extension     | Function                                                                                                             |
|---------------|----------------------------------------------------------------------------------------------------------------------|
| defaults      | Should be enabled, basic compatibility                                                                               |
| pretty-parens | Dim parens in **Indent State**, use rainbow delimiters in **Paren State**                                              |
| smart-yank    | Make yank (paste) preserve indentation in **Indent State** & preserve parens in **Paren State**                        |
| smart-tab     | Make `C-f` & `C-b` on an empty line go to next/previous indentation, possibly inserting extra spaces in the process. |
| paredit       | Introduce some paredit commands from paredit-mode.                                                                   |
| lispy         | Integration with Lispy.                                                                                              |

# Caveats

`parinfer-mode` only works with space-based indentation — tabs will always be replaced with spaces in indent state.

# License

parinferlib.el from [parinfer-elisp](https://github.com/oakmac/parinfer-elisp), is licensed under the [ISC](https://github.com/oakmac/parinfer-elisp/blob/master/LICENSE.md).

```
Copyright (c) 2016, Chris Oakman

Permission to use, copy, modify, and/or distribute this software for any purpose with or without fee is hereby granted, provided that the above copyright notice and this permission notice appear in all copies.
```

[parinfer-mode](https://github.com/DogLooksGood/parinfer-mode) by Shi Tianshu is licensed under GPLv3.

This version is also licensed under GPLv3.
