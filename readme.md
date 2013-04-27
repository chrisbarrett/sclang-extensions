# sclang-extensions

A collection of minor modes that improve your SuperCollider experience within
Emacs. Includes improvements to auto-completion and documentation tooltips (ala
Eldoc).

![Autocomplete popup example](https://raw.github.com/chrisbarrett/sclang-extensions/master/sclang-ac-mode.png)

## sclang-ac-mode

This mode communicates with the SuperCollider process to provide more
intelligent completion candidates.

* Class names
* Instance methods
* Instance variables

These are dynamically generated and context-sensitive - typing `SinOsc.ar` will
no longer prompt you with `Array` for example.

## sclang-doc-mode

Under development. Intended to provide eldoc-style class and method
documentation in the modeline.

## Installation

1. Grab the latest version of the [SuperCollider Emacs mode](https://github.com/supercollider/supercollider/tree/master/editors/scel).

2. Clone this repo. Open Emacs and use `M-x package-install-file` to install
   sclang-extensions-pkg.el

3. Add the following to your init.el:

  ```lisp
  (add-hook 'sclang-mode-hook 'sclang-extensions-mode)
  ```
