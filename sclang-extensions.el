;;; sclang-extensions.el --- Extensions for the SuperCollider Emacs mode.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 2.2.14
;; Package-Requires: ((auto-complete "1.4.0")(s "1.3.1")(dash "1.2.0")(emacs "24.1"))
;; Keywords: sclang supercollider languages tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions for the SuperCollider Emacs mode. Each extension is implemented as
;; a minor-mode; you can enable them individually, or enable all of them by
;; activating `sclang-extensions-mode'.

;;; Installation:

;; Add this mode to your sclang hooks:
;;
;;   (add-hook 'sclang-mode-hook 'sclang-extensions-mode)
;;

;;; Code:

(require 'sclang-ac-mode)
(require 'sclang-doc-mode)
(require 'sclang-post-mode)
(require 'dash)
(autoload 'sclang-eval-region "sclang-interp")
(autoload 'sclang-mode "sclang-mode")
(autoload 'sclang-start "sclang-interp")
(autoload 'sclang-get-process "sclang-interp")
(autoload 'sclang-eval-line "sclang-interp")

;;; ----------------------------------------------------------------------------

(defgroup sclang-extensions nil
  "Extensions to the SuperCollider (sclang) Emacs mode."
  :group 'languages)

(defcustom sclang-bury-post-on-start? t
  "Whether to bury the sclang Post buffer when starting the mode.
The Post buffer becomes much less useful when you use `sclang-post-mode'."
  :group 'sclang-extensions
  :type 'boolean)

(defcustom sclang-extensions-mode-hook nil
  "Hook run after `sclang-extensions-mode' is initialized."
  :group 'sclang-extensions
  :type 'hook)

(defcustom sclang-post-buffer-mode-hook nil
  "Hook run after `sclang-post-buffer-mode' is initialized."
  :group 'sclang-extensions
  :type 'hook)

(defcustom sclang-run-supercollider-if-not-active? t
  "If non-nil, start a SuperCollider process when the mode is activated."
  :group 'sclang-extensions
  :type 'boolean)

;;; The SuperCollider distribution has changed the location of support files on
;;; OSX since the Emacs mode was released. We set these to sane values so you
;;; have any hope of getting sclang working!

(defcustom sclang-osx-app-path "/Applications/SuperCollider/SuperCollider.app"
  "The location of the SuperCollider app on OS X.
Used to set the location of documentation paths."
  :group 'sclang-extensions)

(defcustom sclang-osx-sc-app-support
  (expand-file-name "~/Library/Application Support/SuperCollider")
  "The location of the SuperCollider app support folder on OS X."
  :group 'sclang-extensions)

(defcustom sclang-reassign-osx-paths? t
  "If non-nil, override the default sclang executable and library paths on OS X.
This is necessary because all the supporting files have been moved into the app bundle."
  :group 'sclang-extensions
  :type 'boolean)

;;; ----------------------------------------------------------------------------

;;; Declare special variables from sclang to suppress compiler warnings.
(defvar sclang-post-buffer)
(defvar sclang-extension-path)
(defvar sclang-help-path)
(defvar sclang-program)
(defvar sclang-runtime-directory)

(defun scl:visual-expression-start ()
  "Return the beginning of the current expression.
Ignore trailing semicolons and whitespace.
Return the position of the first non-whitespace char."
  (save-excursion
    (while (and (or (scl:char-before-point-looking-at? (rx (any space "\n" ";"))))
                (not (bobp)))
      (forward-char -1))
    (-when-let (pos (scl:expression-start-pos))
      (goto-char pos)
      (when (search-forward-regexp (rx (not (any space "\n"))) nil t)
        (unless (bobp)
          (forward-char -1))))
    (point)))

;;;###autoload
(defun sclang-eval-last-expression ()
  "Evaluate the sclang expression before point."
  (interactive)
  (->> (buffer-substring-no-properties (scl:visual-expression-start) (point))
    (scl:blocking-eval-string)
    (scl:print-post-message)))

;;;###autoload
(defun sclang-expression-start ()
  "Move to the start of the sclang expression before point."
  (interactive)
  (-when-let (pos (scl:visual-expression-start))
    (goto-char pos)))

;;;###autoload
(defun sclang-eval-dwim ()
  "Perform a context-sensitive evaluation action.
Either eval the current region or the top level grouping at point."
  (interactive)
  (message nil)
  (if (region-active-p)
      (progn (sclang-eval-region)
             (deactivate-mark))
    (save-excursion
      (mark-defun)
      (sclang-eval-region))))

;;;###autoload
(defvar sclang-extensions-mode-map
  (let ((km (make-keymap)))
    (define-key km (kbd "M-a")     'sclang-expression-start)
    (define-key km (kbd "C-x C-e") 'sclang-eval-last-expression)
    (define-key km (kbd "C-c C-c") 'sclang-eval-dwim)
    (define-key km (kbd "C-c C-z") 'sclang-switch-to-post)
    (define-key km (kbd "M-q")     'indent-buffer)
    (define-key km (kbd "s-.")     'sclang-main-stop)
    (define-key km (kbd "C-c C-l") 'sclang-eval-document)
    km))

;;;###autoload
(defun sclang-switch-to-post ()
  "Switch between the Post buffer and the last sclang buffer."
  (interactive)
  (-if-let (win (->> (window-list)
                  (--first (with-current-buffer (window-buffer it)
                             (derived-mode-p 'sclang-post-buffer-mode)))))
    (select-window win)
    (->> (buffer-list)
      (--first (with-current-buffer it
                 (derived-mode-p 'sclang-post-buffer-mode)))
      (switch-to-buffer))))

;;;###autoload
(defun sclang-switch-to-src ()
  "Switch to the last sclang source file."
  (interactive)
  (-if-let (win (->> (window-list)
                  (--first (with-current-buffer (window-buffer it)
                             (derived-mode-p 'sclang-mode)))))
    (select-window win)
    (->> (buffer-list)
      (--first (with-current-buffer it
                 (derived-mode-p 'sclang-mode)))
      (switch-to-buffer))))


(defun scl:bury-post-buffer ()
  "Hide the SuperCollider Post buffer."
  (--each (--filter (equal sclang-post-buffer (buffer-name (window-buffer it)))
                    (window-list))
    (delete-window it)))

(defun sclang-set-osx-paths ()
  "Set sclang paths to modern values on OS X."
  (when (equal system-type 'darwin)
    (let ((macos (concat sclang-osx-app-path "/Contents/MacOS"))
          (resources (concat sclang-osx-app-path "/Contents/Resources")))
      (setq
       sclang-runtime-directory (concat resources "/SCClassLibrary")
       sclang-program (if (file-exists-p (concat macos "/sclang"))
                          (concat macos "/sclang") ; SuperCollider 3.7.x
                        (concat resources "/sclang")) ; SuperCollider 3.6.x
       sclang-extension-path (list (concat sclang-osx-sc-app-support "/Extensions"))

       sclang-help-path
       (list (concat sclang-osx-sc-app-support "/Help")
             (concat sclang-osx-sc-app-support "/Help/Reference")
             (concat sclang-osx-sc-app-support "/Help/Classes"))))))

;;; Define a distinct mode for the sclang post buffer so that it can be
;;;individually customised.

;;;###autoload
(define-derived-mode sclang-post-buffer-mode special-mode
  "SCPost"
  "Major mode for sclang post buffer."
  (read-only-mode -1)
  (local-set-key (kbd "C-c C-z") 'sclang-switch-to-src))

;;;###autoload
(define-minor-mode sclang-extensions-mode
  "Enable all extensions to the sclang Emacs mode."
  nil " sclang+" sclang-extensions-mode-map
  (cond

   ;; Enable mode --------------------------------------------------------------
   (sclang-extensions-mode

    ;; Configure paths for OS X.
    (when sclang-reassign-osx-paths?
      (sclang-set-osx-paths))

    ;; sclang-mode uses indent-tabs-mode by default - WHYYYY!?!
    (setq-local indent-tabs-mode nil)

    ;; Enable associated modes.
    (sclang-ac-mode +1)
    (sclang-doc-mode +1)
    (sclang-post-mode +1)

    ;; Start up SuperCollider.
    (when sclang-run-supercollider-if-not-active?
      (unless (or (equal (buffer-name) sclang-post-buffer)
                  (sclang-get-process))
        (sclang-start)))

    ;; Hide the post buffer.
    (when sclang-bury-post-on-start?
      (scl:bury-post-buffer)))

   ;; Disable mode -------------------------------------------------------------
   (t
    ;; Deactivate minor modes.
    (remove-hook 'sclang-mode-hook 'scl:bury-post-buffer t)
    (sclang-ac-mode -1)
    (sclang-post-mode -1)
    (sclang-doc-mode -1))))

(defadvice sclang-mode (around use-sclang-post-buffer-mode activate)
  "Use sclang-post-buffer-mode instead of sclang-mode for the post buffer."
  (if (equal sclang-post-buffer (buffer-name))
      (sclang-post-buffer-mode)
    ad-do-it))

(provide 'sclang-extensions)

;;; sclang-extensions.el ends here
