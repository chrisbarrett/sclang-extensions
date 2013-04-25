# sclang-ac-mode

A minor mode that improves the auto-complete behaviour for the SuperCollider Emacs mode.

## Motivation

The SuperCollider major mode for Emacs uses a dictionary of classes to provide
autocompletion candidates. This is a brittle mechanism that can easily fall
out-of-sync with changes to the SuperCollider libraries. It will also fail to
complete any classes added by 3rd-party libraries.

This mode communicates with the SuperCollider process to provide more
intelligent completion candidates.

* Class names
* Instance methods
* Instance variables

These are dynamically generated and context-sensitive - typing `SinOsc.ar` will
no longer prompt you with `Array`!

## Installation

1. Use `M-x package-install` to install the following dependencies:

  * s
  * dash
  * auto-complete

2. Use `M-x package-install-file` to install sclang-ac-mode.el

3. Add the following to your init.el:

  ```lisp
  (add-hook 'sclang-mode-hook 'sclang-ac-mode)
  ```
