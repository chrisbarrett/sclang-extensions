;;; sclang-ac-mode.el --- Improved auto-complete for SuperCollider.

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

;; Provides `sclang-ac-mode', a minor-mode that overrides the default
;; auto-complete behavior for sclang-mode.  Communicates with the SuperCollider
;; runtime to provide more intelligent auto-completion.

;;; Code:

(require 'dash)
(require 's)
(require 'cl-lib)
(require 'auto-complete)
(require 'sclang-extensions-utils)

;;; ----------------------------------------------------------------------------

(defcustom sclang-ac-mode-hook nil
  "Hook run when `sclang-ac-mode' has started."
  :group 'sclang-extensions
  :type 'hook)

;;; ----------------------------------------------------------------------------
;;; Completion sources.
;;
;; Completion sources that require a reference to a class (i.e. methods,
;; instance vars) use the `scl:last-class' variable. This ensures that we know
;; which class to operate on regardless of buffer insertions caused by the
;; completion.

(defvar scl:last-class nil
  "The class to use for completion candidates.")

(defconst scl:bullet "•")
(defconst scl:ellipsis "…")

(cl-defun scl:ellipsize (str &optional (maxlen 30))
  "Ellipsize string STR if it is longer than MAXLEN."
  (cond

   ;; Return unchanged if less than maxlen.
   ((<= (length str) maxlen) str)

   ;; Abbreviate lists.
   ((ignore-errors (listp (read str)))
    (->> (substring str 1 (- maxlen 3))
      (s-split-words)
      (butlast)
      (s-join " ")
      (s-prepend "[")
      (s-append (format " %s]" scl:ellipsis))))

   ;; Trim and ellipsize.
   (t
    (s-append scl:ellipsis (substring str 0 (1- maxlen))))))

(defun scl:method-bullets (method-arg-info)
  "Build a bulleted list describing a method's arguments."
  (when (and method-arg-info (listp method-arg-info))
    (format "\n\narguments:\n\n%s"
            (->> method-arg-info
              (--map (format "%s: \t%s" (car it) (cadr it)))
              (s-join (format "\n\n %s " scl:bullet))
              (s-prepend (format " %s " scl:bullet))))))

(cl-defun scl:selected-method-doc ((arglist owner)
                                   &optional (name (ac-selected-candidate)))
  "Show documentation for the currently selected method in the `ac-menu'."
  (s-concat
   ;; Display name.
   (format "%s.%s\n\n" owner name)
   ;; Display arglist.
   (unless (s-blank? arglist)
     (->> (scl:arguments arglist) (s-join ", ") (format "(%s)")))
   ;; Display arglist details.
   (scl:method-bullets (scl:method-arg-info owner name))))

(cl-defun scl:method-item ((name arglist owner))
  "Stringify and process the elements of an sclang method item."
  (let ((sym (eval name)))
    (when (symbolp sym)
      (list (symbol-name sym)
            (eval arglist)
            (scl:ensure-non-meta-class (symbol-name owner))))))

(cl-defun scl:class-doc-subclasses (class &optional (maxlen 5))
  "Return a list of subclasses. It will be ellipsized if longer than MAXLEN"
  (let* ((subclasses (scl:subclasses class))
         ;; Show MAXLEN subclasses before ellipsizing.
         (sub-str (->> subclasses
                    (-take maxlen)
                    (s-join (concat "\n " scl:bullet " "))
                    (s-prepend (concat "\n " scl:bullet " "))))
         (sub-str (if (< maxlen (length subclasses))
                      (s-append "\n  …" sub-str)
                    sub-str)))
    (when subclasses
      (concat "\n\nsubclasses:" sub-str))))

(defun scl:class-documentation (class)
  "Create an auto-complete documentation for CLASS."
  (let ((super (s-join " < " (scl:superclasses class))))
    (s-concat
     class
     ;; Summarize class.
     (-when-let (summary (scl:class-summary class))
       (concat ":\n" summary))
     ;; Display inheritance chain.
     (unless (s-blank? super)
       (format "\n\ninheritance chain:\n%s < %s" class super))
     ;; List subclasses.
     (scl:class-doc-subclasses class))))

(defun scl:class-defines? (class name)
  "Return a cons of (CLASS . NAME) if CLASS defines a method or var NAME."
  (when (-> (-concat (scl:instance-vars class) (scl:methods class))
          (-contains? name))
    (cons class name)))

(defun scl:find-declaring-class (class name)
  "Walk the class hierarchy from CLASS, searching for which class defines NAME."
  (->> (cons class (scl:superclasses class))
    (reverse)
    (--first (scl:class-defines? it name))))

(cl-defun scl:selected-var-doc (var-name &optional (class scl:last-class))
  "Get the documentation for VAR-NAME."
  (format "%s.%s" (scl:find-declaring-class class var-name) var-name))

(defun scl:blank-line? ()
  "Non-nil if the current line is only spaces or tabs."
  (s-matches? (rx bol (* (any space "\t")) eol)
              (buffer-substring (line-beginning-position)
                                (line-end-position))))

(ac-define-source sclang-classes
  '((candidates . (scl:logged
                    (unless (equal sclang-post-buffer (buffer-name))
                      (when (or (not (scl:looking-at-member-access?))
                                (scl:blank-line?))
                        (scl:all-classes)))))
    (document   . scl:class-documentation)
    (symbol     . "s")
    (limit      . nil)
    (cache)))

(ac-define-source sclang-toplevel-functions
  '((candidates . (scl:logged
                    (unless (or (equal sclang-post-buffer (buffer-name))
                                (scl:looking-at-member-access?))
                      (-map 'scl:method-item (scl:methods "AbstractFunction")))))
    (document   . scl:selected-method-doc)
    (symbol     . "f")
    (limit      . nil)))

(ac-define-source sclang-methods
  '((candidates . (scl:logged
                    (unless (equal sclang-post-buffer (buffer-name))
                      (->> (scl:all-methods scl:last-class)
                        (-map 'scl:method-item)
                        (-remove 'null)))))
    (document   . scl:selected-method-doc)
    (prefix     . ac-prefix-default)
    (symbol     . "f")
    (limit      . nil)
    (requires   . -1)))

(ac-define-source sclang-ivars
  '((candidates . (scl:logged
                    (unless (equal sclang-post-buffer (buffer-name))
                      (scl:instance-vars scl:last-class))))
    (prefix     . ac-prefix-default)
    (document   . scl:selected-var-doc)
    (symbol     . "v")
    (limit      . nil)
    (requires   . -1)))

(ac-define-source sclang-keyword-args
  '((candidates . (scl:logged
                    (ignore-errors
                      (-when-let (info (scl:method-for-arglist-at-point))
                        (destructuring-bind (_name arglist _owner) info
                          (--map (concat it ":") (scl:arguments arglist)))))))
    (symbol     . "k")))

;;; ----------------------------------------------------------------------------

;;;###autoload
(defun sclang-electric-dot ()
  "Insert a dot and access members for the sclang expr before point."
  (interactive)

  ;; Update the reference to the last class before starting completion.
  (-when-let (k (scl:class-of-thing-at-point))
    (setq scl:last-class k))

  (insert ".")

  (let ((ac-expand-on-auto-complete t))
    (auto-complete '(ac-source-sclang-ivars ac-source-sclang-methods))))

(defvar sclang-ac-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd ".") 'sclang-electric-dot)
    map)
  "Keymap for sclang-ac-mode.
\\{sclang-ac-mode-map}")

;;;###autoload
(define-minor-mode sclang-ac-mode
  "Minor mode that provides more intelligent auto-complete behaviour for SuperCollider."
  nil nil sclang-ac-mode-map
  (when sclang-ac-mode
    (add-to-list 'ac-modes 'sclang-mode)
    (auto-complete-mode +1)
    (setq-local ac-sources '(ac-source-sclang-ivars
                             ac-source-sclang-classes
                             ac-source-sclang-methods
                             ac-source-sclang-keyword-args
                             ac-source-sclang-toplevel-functions))))

(provide 'sclang-ac-mode)

;;; sclang-ac-mode.el ends here
