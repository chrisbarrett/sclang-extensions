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

(cl-defun scl:method-desc-at-point (&key (formatter 'scl:method-desc))
  "Return a propertized arglist of the method at point if available."
  (-when-let*
      ((class (or (scl:class-of-thing-at-point) "AbstractFunction"))
       (method (scl:symbol-near-point))
       (info
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
    (funcall formatter info)))

(defun scl:propertised-usage (pos-index arglist)
  "Return a propertized arglist, where the argument at point is in bold."
  (format " (%s)"
          (->> (s-split-words arglist)
            (--map-indexed
             (if (equal pos-index it-index)
                 (propertize it 'face 'font-lock-variable-name-face)
               it))
            (s-join ", "))))

(defun scl:list-comma-indices (list-str)
  "Return the indices of commas at the top level of LIST-STR.
LIST-STR is a string representation of a list."
  (with-temp-buffer
    (insert list-str)
    (goto-char (point-min))
    (let ((indices nil))
      (while (search-forward-regexp "," nil t)
        ;; Find commas at the root level of the list.
        (when (equal (scl:surrounding-braces)
                     (cons (point-min) (point-max)))
          (setq indices (cons (point) indices))))
      (nreverse indices))))

(cl-defun scl:position-in-list (&optional (pos (point)))
  "When inside an sclang list, return the index of the element at point."
  (-when-let* ((bounds (scl:surrounding-braces pos)))
    (->> (buffer-substring (car bounds) (cdr bounds))
      (scl:list-comma-indices)
      (--map (1- (+ it (car bounds))))
      (--take-while (<= it (point)))
      (length))))

;;; TODO: Test for keywords when highlighting arglist.
(defun scl:method-desc-for-arglist ()
  "When inside an arglist, return a description of the corresponding method."
  (save-excursion
    ;; Move off leading braces and into arglist.
    (when (scl:char-before-point-looking-at? "(") (forward-char))

    (-when-let* ((arg-index (scl:position-in-list (point)))
                 (bounds    (scl:surrounding-braces)))
      (goto-char (1- (car bounds)))
      (scl:method-desc-at-point
       :formatter
       (lambda (info)
         (destructuring-bind (name arglist owner) info
           (concat
            ;; Declaring class name
            (propertize owner 'face 'font-lock-type-face)
            "."
            ;; Method name
            (propertize name 'face 'font-lock-function-name-face)
            ;; Format the arglist. Color individual items.
            (concat " " (scl:propertised-usage arg-index arglist)))))))))

(defun scl:minibuffer-doc ()
  "Display the appropriate documentation for the symbol at point."
  ;; If any of these fail, we still want to try the others.
  (or (ignore-errors (scl:class-desc-at-point))
      (ignore-errors (scl:method-desc-at-point))
      (ignore-errors (scl:method-desc-for-arglist))))

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
