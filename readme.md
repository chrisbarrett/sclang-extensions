# sclang-extensions

A collection of minor modes that improve your SuperCollider experience within
Emacs. Includes improvements to auto-completion and documentation tooltips (ala
Eldoc).

![Autocomplete popup example](https://raw.github.com/chrisbarrett/sclang-extensions/master/assets/sclang-ac-mode.png)

## sclang-ac-mode

This mode communicates with the SuperCollider process to provide more
intelligent completion candidates.

* Class names
* Instance methods
* Instance variables

These are dynamically generated and context-sensitive - typing `SinOsc.ar` will
no longer prompt you with `Array` for example.

## sclang-doc-mode

Shows information about the class or method under the cursor in the minibuffer.
This lets you see arglists and class descriptions without trawling through the
SuperCollider help system.

## sclang-post-mode

Displays feedback from SuperCollider in the minibuffer. This means you no longer
have to keep the Post window open all the time.

---

# Installation

`sclang-extensions` is available on [MELPA](http://melpa.milkbox.net/). This is
the easiest way to install.

If you haven't set up MELPA, you'll need to add the following to your init.el

```lisp
;;; Initialize packages.

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
```

Once MELPA is configured:

1. `M-x package-install sclang-extensions`.

2. Configure your init.el:

   ```lisp
   (add-hook 'sclang-mode-hook 'sclang-extensions-mode)
   ```

You will need to install the SuperCollider language mode for Emacs if you do not
already have it. Grab the latest version
[here](https://github.com/supercollider/supercollider/tree/master/editors/scel).

# Development

You will need *cask*, *make* and *git* to build the project.

1. Install [Cask](https://github.com/rejeep/cask.el):

    ```
    curl -fsSkL https://raw.github.com/rejeep/cask.el/master/go | sh
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
