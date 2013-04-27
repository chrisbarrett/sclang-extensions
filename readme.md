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

# Installation

## Package

`sclang-extensions` is available on [MELPA](http://melpa.milkbox.net/): `M-x
package-install sclang-extensions`. If you haven't set up MELPA, add the
following to your init.el:

```lisp
;;; Initialize packages.

(require 'package)
(add-to-list 'package-archives '("melpa"     . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
```

## Manual

1. Install [Carton](https://github.com/rejeep/carton):

   ```
   curl -fsSkL https://raw.github.com/rejeep/carton/master/go | sh
   ```

2. Clone and install with `make`:

   ```
   cd
   git clone git@github.com:chrisbarrett/sclang-extensions.git
   cd sclang-extensions
   make
   ```

3. Configure your init.el:

   ```lisp
   (add-hook 'sclang-mode-hook 'sclang-extensions-mode)
   ```

You will need to install the SuperCollider language mode for Emacs if you do not
already have it. Grab the latest version
[here](https://github.com/supercollider/supercollider/tree/master/editors/scel).
