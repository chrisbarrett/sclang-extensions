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

(defcustom sclang-doc-mode-hook nil
  "Hook run when `sclang-doc-mode' has initialized."
  :group 'sclang-extensions
  :type 'hook)

;;; ----------------------------------------------------------------------------

(cl-defun scl:class-desc-at-point (&optional (class (symbol-name (symbol-at-point))))
  "Return a propertized string describing CLASS."
  (when (-contains? (scl:all-classes) class)
    (concat
     ;; Class name.
     (propertize class 'face 'font-lock-type-face)
     ;; Description.
     ": " (scl:class-summary class))))

(defun* scl:method-desc ((name arglist owner))
  "Return a propertized help string for the given method info."
  (concat
   (propertize owner 'face 'font-lock-type-face)
   "."
   (propertize name 'face 'font-lock-function-name-face)
   (->> (scl:arguments arglist) (s-join ", ") (format " (%s)"))))

(defun scl:symbol-near-point ()
  "Like `symbol-at-point', but allows whitespace to the left of POINT."
  (save-excursion
    (or (symbol-at-point)
        (progn
          (search-backward-regexp (rx (not space))
                                  (line-beginning-position) t)
          (symbol-at-point)))))

(defun scl:method-at-point ()
  "Return a method info for the method at point."
  (-when-let* ((class (or (scl:class-of-thing-at-point) "AbstractFunction"))
               (method (scl:symbol-near-point)))
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

(cl-defun scl:method-desc-at-point ()
  "Return a propertized arglist of the method at point if available."
  (scl:method-desc (scl:method-at-point)))

(defun scl:propertised-usage (arglist pos-index current-kw)
  "Return a propertized arglist, where the argument at point is in bold.

* ARGLIST is the argument list to format. It should be a string.

* POS-INDEX is the index of the element at point.

* CURRENT-KW is the keyword argument at point, if any."
  (->> (scl:arguments arglist)
    (--map-indexed
     (cond
      ;; Propertise keyword argument.
      ((and current-kw (equal current-kw (read it)))
       (propertize it 'face 'font-lock-variable-name-face))
      ;; Propertise positional argument.
      ((and (not current-kw) (equal pos-index it-index))
       (propertize it 'face 'font-lock-variable-name-face))
      ;; Return unchanged.
      (t it))
     )
    (s-join ", ")
    (format "(%s)")))

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

(cl-defun scl:argument-start-position
    (&optional (context (scl:surrounding-braces)))
  "Find the starting position of the current argument to the method call at point."
  (save-excursion
    (-when-let (pos (scl:expression-start-pos)))
    (when (search-backward-regexp
           (rx "," (* (or "\n" space)) (group nonl))
           (car (scl:surrounding-braces)) t)
      (if (equal context (scl:surrounding-braces))
          (1+ (point))
        (scl:argument-start-position context)))))

(defun scl:extract-keyword-at-point ()
  "Extract the entire keyword at point."
  ;; Move to start of word.
  (save-excursion
    (when (symbol-at-point) (backward-sexp))
    ;; Find the keyword at point. Return it without the colon.
    (-when-let (colon (save-excursion
                        (search-forward ":" (line-end-position) t)))
      (->> (buffer-substring-no-properties (point) colon)
        (s-chop-suffix ":" )
        (read)))))

(defun scl:method-keyword-at-point ()
  "When looking at a keyword argument in a method call, return that keyword."
  (save-excursion
    ;; Find the start of the argument expression.
    (-when-let (pos (scl:argument-start-position))
      (goto-char pos))
    (search-forward-regexp (rx (not (any "\n" space))))
    ;; Test whether the current symbol is a keyword.
    (-when-let* ((kw (scl:extract-keyword-at-point))
                 (method (scl:method-for-arglist-at-point)))
      ;; Return the KW if it is a parameter name for the method at point.
      (destructuring-bind (_name arglist _owner) method
        (cl-find kw (read arglist))))))

(defun scl:method-for-arglist-at-point ()
  "If point is inside a method call arglist, return the method being called."
  (save-excursion
    ;; Move off leading braces and into arglist.
    (when (scl:char-before-point-looking-at? "(") (forward-char))

    (-when-let* ((arg-index (scl:position-in-list (point)))
                 (bounds (scl:surrounding-braces)))
      (goto-char (1- (car bounds)))
      (scl:method-at-point))))

(defun scl:method-desc-for-arglist ()
  "When inside an arglist, return a description of the corresponding method."
  (save-excursion
    (when (scl:char-before-point-looking-at? "(") (forward-char))
    (-when-let (info (scl:method-for-arglist-at-point))
      (destructuring-bind (name arglist owner) info
        (concat
         ;; Declaring class name
         (propertize owner 'face 'font-lock-type-face)
         "."
         ;; Method name
         (propertize name 'face 'font-lock-function-name-face)
         ;; Format the arglist. Color individual items.
         (concat " " (scl:propertised-usage
                      arglist
                      (scl:position-in-list (point))
                      (scl:method-keyword-at-point))))))))

(defun scl:minibuffer-doc ()
  "Display the appropriate documentation for the symbol at point."
  ;; If any of these fail, we still want to try the others.
  (or (ignore-errors (scl:class-desc-at-point))
      (ignore-errors (scl:method-desc-at-point))
      (ignore-errors (scl:method-desc-for-arglist))))

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
      (eldoc-mode +1)))
   ;; Deactivate mode.
   (t
    (eldoc-mode -1)
    (kill-local-variable 'eldoc-documentation-function))))

(provide 'sclang-doc-mode)

;;; sclang-doc-mode.el ends here
