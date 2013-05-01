;;; sclang-doc-mode.el --- Minibuffer documentation for SuperCollider.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

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

;; Minibuffer documentation for SuperCollider.

;;; Installation:

;; (add-hook 'sclang-mode-hook 'enable-sclang-doc)

;;; Code:

(require 'dash)
(require 's)
(require 'cl-lib)
(require 'sclang-extensions-utils)
(require 'eldoc)

;;; ----------------------------------------------------------------------------

(cl-defun scl:class-desc-at-point (&optional (class (symbol-name (symbol-at-point))))
  "Return a propertized string describing CLASS."
  (when (-contains? (scl:all-classes) class)
    (concat
     ;; Class name.
     (propertize class 'face 'font-lock-type-face)
     ;; Description.
     ": " (scl:class-summary class))))

(defun scl:propertised-arglist (arglist)
  "Process the given ARGLIST string and apply text properties."
  (format " (%s)"
          (->> (s-split-words arglist)
            (--map (propertize it 'face 'font-lock-variable-name-face))
            (s-join ", "))))

(defun* scl:method-desc ((name arglist owner))
  "Return a propertized help string for the given method info."
  (concat
   ;; Declaring class name
   (propertize owner 'face 'font-lock-type-face)
   "."
   ;; Method name
   (propertize name 'face 'font-lock-function-name-face)
   ;; Format the arglist. Color individual items.
   (concat " " (scl:propertised-arglist arglist))))

(defun scl:symbol-near-point ()
  "Like `symbol-at-point', but allows whitespace to the left of POINT."
  (save-excursion
    (or (symbol-at-point)
        (progn
          (search-backward-regexp (rx (not space))
                                  (line-beginning-position) t)
          (symbol-at-point)))))

(cl-defun scl:method-desc-at-point
    (&optional (class (or (scl:class-of-thing-at-point) "AbstractFunction"))
               (method (scl:symbol-near-point)))
  "Return a propertized arglist of the method at point if available."
  (-when-let
      (info (and class method
                 ;; Try the class as is, as well as the meta-class.
                 (or
                  (->> (scl:all-methods class)
                    (-map 'scl:method-item)
                    (-remove 'null)
                    (--first (equal (car it) (symbol-name method))))

                  (->> (scl:all-methods (concat "Meta_" class))
                    (-map 'scl:method-item)
                    (-remove 'null)
                    (--first (equal (car it) (symbol-name method)))))))
    (scl:method-desc info)))

(defun scl:method-desc-before-point ()
  "When inside an arglist, return a description of the corresponding method."
  (save-excursion
    (while (not (scl:looking-at-member-access?))
      (forward-char -1))
    (scl:method-desc-at-point)))

(defun scl:minibuffer-doc ()
  "Display the appropriate documentation for the symbol at point."
  ;; If any of these fail, we still want to try the others.
  (or (ignore-errors (scl:class-desc-at-point))
      (ignore-errors (scl:method-desc-at-point))
      (ignore-errors (scl:method-desc-before-point))))

(defvar sclang-doc-mode-hook)

;;;###autoload
(define-minor-mode sclang-doc-mode
  "Displays minibuffer documentation for the SuperCollider symbol at point."
  nil nil nil
  (cond
   ;; Enable mode.
   (sclang-doc-mode
    ;; Don't do anything if this is the Post buffer.
    (unless (and (boundp 'sclang-post-buffer)
                 (equal sclang-post-buffer (buffer-name)))
      (make-local-variable 'eldoc-documentation-function)
      (setq eldoc-documentation-function 'scl:minibuffer-doc)
      (eldoc-mode +1)
      (run-hooks 'sclang-doc-mode-hook)))
   ;; Deactivate mode.
   (t
    (eldoc-mode -1)
    (kill-local-variable 'eldoc-documentation-function))))

(provide 'sclang-doc-mode)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; sclang-doc-mode.el ends here
