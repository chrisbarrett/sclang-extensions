# sclang-ac-mode

A minor mode that improves the auto-complete behaviour for the SuperCollider Emacs mode.

![Autocomplete popup example](https://raw.github.com/chrisbarrett/sclang-ac-mode/master/sclang-ac-mode.png)

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

1. Grab the latest version of the [SuperCollider Emacs bindings](https://github.com/supercollider/supercollider/tree/master/editors/scel).

2. Use `M-x package-install-file` to install sclang-ac-mode.el

3. Add the following to your init.el:

  ```lisp
  (add-hook 'sclang-mode-hook 'sclang-ac-mode)
  ```

## TODO

* Figure out how to show all members on electric-dot
* Figure out why some instance members don't show consistently.
