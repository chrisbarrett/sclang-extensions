;;; sclang-ac-mode.el --- Improved auto-complete for SuperCollider.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.2.2
;; Package-Requires: ((auto-complete "1.4.0")(popup "0.5.0")(s "1.3.1")(dash "1.2.0")(emacs "24.1"))
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

;; Provides `sclang-ac-mode', a minor-mode that overrides the default
;; auto-complete behavior for sclang-mode.  Communicates with the SuperCollider
;; runtime to provide more intelligent auto-completion.

;;; Installation:

;; 1. Install the package dependencies, listed above.
;;
;; 2. Install this package with `M-x package-install-file`
;;
;; 3. Add this mode to your sclang hooks:
;;    (add-hook 'sclang-mode-hook 'sclang-ac-mode)

;;; Code:

(require 'dash)
(require 's)
(require 'popup)
(require 'auto-complete)
(autoload 'sclang-eval-string "sclang-help")
(autoload 'thing-at-point-looking-at "thingatpt")

(defgroup sclang-ac nil
  "Improved auto-complete for SuperCollider."
  :group 'languages
  :prefix "sclang-ac")

;;; Customizable vars.

(defface sclang-ac-defined-member-face
  '((t (:inherit ac-candidate-face
                 :background "sandybrown" :foreground "black")))
  "Used for unselected members defined by the current class in the completion menu."
  :group 'sclang-ac)

(defface sclang-ac-defined-member-selection-face
  '((t (:inherit ac-selection-face :background "coral3")))
  "Used for selected members defined by the current class in the completion menu."
  :group 'sclang-ac)

(defcustom sclang-ac-popup-help-delay
  (if (boundp 'ac-quick-help-delay) ac-quick-help-delay 0.25)
  "The number of seconds to wait before displaying help for a completion item."
  :group 'sclang-ac)

(defcustom sclang-ac-verbose
  nil
  "If non-nil, print extra debugging info to the messages buffer."
  :group 'sclang-ac)

;;; ----------------------------------------------------------------------------

(defmacro slc:logged (&rest body)
  "Like `progn', but logs the result to messages if `sclang-ac-verbose' is non-nil."
  (declare (indent 0))
  (let ((result (cl-gensym)))
    `(let ((,result (progn ,@body)))
       (when sclang-ac-verbose
         (message "[sclang-ac]: %s" ,result))
       ,result)))

(defun* slc:blocking-eval-string (expr &optional (timeout-ms 100))
  "Ask SuperCollider to evaluate the given string EXPR. Wait a maximum TIMEOUT-MS."
  (unless (s-blank? expr)
    (let ((result nil)
          (elapsed 0)
          ;; Prevent expressions from crashing sclang.
          (fmt (format "try { Emacs.message((%s).asCompileString) } {|err|}" expr))
          )
      ;; SuperCollider will eval the string and then call back with the result.
      ;; We rebind Emacs' `message' action to intercept the response.

      (flet ((message (str &rest _) (setq result str)))

        (sclang-eval-string fmt)

        ;; Block until we receive a response or the timeout expires.
        (while (and (not result) (> timeout-ms elapsed))
          (sleep-for 0 10)
          (setq elapsed (+ 10 elapsed)))
        result))))

(defun slc:deserialize (str)
  "Parse the SuperCollider response STR."
  (when str
    (->> str
      ;; Parse SuperCollider arrays to lists.
      (s-replace "," "")
      (s-replace "[" "(")
      (s-replace "]" ")")
      (read))))

(defun slc:methods (class)
  "Return a list of methods implemented by CLASS."
  (->> (concat class ".methods.collect {|m| [m.name, m.argList, m.ownerClass] } ")
    (slc:blocking-eval-string)
    (slc:deserialize)))

(defun slc:instance-vars (class)
  "Return a list of the instance variables of CLASS."
  (->> (concat class ".instVarNames.collect(_.asString)")
    (slc:blocking-eval-string)
    (slc:deserialize)))

(defun slc:class-vars (class)
  "Return a list of the class variables of CLASS."
  (->> (concat class ".classVarNames.collect(_.asString)")
    (slc:blocking-eval-string)
    (slc:deserialize)))

(defun slc:superclasses (class)
  "Return a list of superclasses for CLASS."
  (->> (concat class ".superclasses")
    (slc:blocking-eval-string)
    (slc:deserialize)
    (-map 'symbol-name)))

(defun slc:subclasses (class)
  "Return the direct subclasses of CLASS."
  (->> (concat class ".subclasses")
    (slc:blocking-eval-string)
    (slc:deserialize)
    (-map 'symbol-name)))

(defun slc:class-of (expr)
  "Evaluate EXPR and return the class of the result."
  (slc:blocking-eval-string (format "(%s).class" expr)))

(defun slc:all-classes ()
  "Return the list of classes known to SuperCollider."
  (->> "Class.allClasses.asArray"
      (slc:blocking-eval-string)
      (s-replace "class" "")
      (slc:deserialize)
      (-map 'symbol-name)))

(defun slc:looking-at-member-access? ()
  "Return point if not looking at a member access."
  (when (s-contains? "." (thing-at-point 'line ))
    (point)))

;;; ----------------------------------------------------------------------------
;;; Completion sources.
;;
;; Completion sources that require a reference to an object (i.e. methods,
;; instance vars) use the `slc:last-class' variable. This ensures that we know
;; which class to operate on regardless of buffer insertions caused by the
;; completion.

(defvar slc:last-class nil
  "The class to use for completion candidates.")

(defun* slc:method-item (class (name arglist owner))
  "Return a popup item for the corresponding sclang method item."
  (let ((name (symbol-name (eval name)))
        (arglist (eval arglist))
        (owner (symbol-name owner)))
    (popup-make-item
     name
     :symbol "f"

     :document
     (s-concat
      (format "%s.%s\n\n" owner name)
      (unless (s-blank? arglist) arglist))

     ;; Use special faces var methods defined by the current class.
     :face
     (when (equal class owner)
         'sclang-ac-defined-member-face
       'popup-item-face)

     :selection-face
     (if(equal class owner)
         'sclang-ac-defined-member-selection-face
       'popup-selection-face))))

(defun slc:class-documentation (class)
  "Format a help popup for CLASS."
  (let* ((super (s-join " < " (slc:superclasses class)))
         ;; Take a set number of subclasses before ellispsizing.
         (max-subs 5)
         (bullet "\n• ")
         (subclasses (slc:subclasses class))
         (sub-str    (->> subclasses
                       (-take max-subs)
                       (s-join bullet )
                       (s-prepend bullet))))
    (s-concat
     ;; Show either the class name or its inheritance hierarchy.
     (if (s-blank? super) class (format "%s: %s" class super))
     ;; Show list of subclasses. It will be ellipsized if longer than MAX-SUBS.
     (when subclasses
       (format "\n\nsubclasses:%s"
               (if (< max-subs (length subclasses))
                   (s-append "\n  …" sub-str)
                 sub-str))))))

(ac-define-source sclang-classes
  '((candidates . (slc:logged
                    (unless (slc:looking-at-member-access?)
                      (slc:all-classes))))
    (document   . slc:class-documentation)
    (symbol     . "s")
    (limit      . nil)))

(ac-define-source sclang-toplevel-functions
  '((candidates . (slc:logged
                    (unless (slc:looking-at-member-access?)
                      (--map (slc:method-item slc:last-class it)
                             (slc:methods "AbstractFunction")))))
    (symbol     . "f")
    (limit      . nil)))

(ac-define-source sclang-methods
  '((candidates . (slc:logged
                    (--map (slc:method-item slc:last-class it)
                           (slc:methods slc:last-class))))
    (prefix     . ac-prefix-default)
    (limit      . nil)
    (requires   . -1)))

(ac-define-source sclang-ivars
  '((candidates . (slc:logged
                    (slc:instance-vars slc:last-class)))
    (prefix     . ac-prefix-default)
    (symbol     . "v")
    (limit      . nil)
    (requires   . -1)))

(defun slc:class-of-thing-at-point ()
  "Return the class of the sclang expression at point."
  (slc:logged
    (->> (buffer-substring-no-properties (line-beginning-position) (point))
      ;; Remove trailing dot-accessor.
      (s-trim)
      (s-chop-suffix ".")
      (slc:class-of))))

;;;###autoload
(defun sclang-electric-dot ()
  "Insert a dot and access members for the sclang expr before point."
  (interactive)

  ;; Update the reference to the last class before starting completion.
  (-when-let (k (slc:class-of-thing-at-point))
    (setq slc:last-class k))

  (insert ".")

  (let ((ac-expand-on-auto-complete t))
    (auto-complete '(ac-source-sclang-ivars ac-source-sclang-methods))))

;;; ----------------------------------------------------------------------------

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

  ;; Override the sources defined by sclang-mode.
  (setq ac-sources '(ac-source-yasnippet
                     ac-source-sclang-ivars
                     ac-source-sclang-classes
                     ac-source-sclang-methods
                     ac-source-sclang-toplevel-functions))

  (auto-complete-mode +1))

(provide 'sclang-ac-mode)

;;; NB: We need to use `flet', an obsolete macro. Suppress the usage warning.

;; Local Variables:
;; lexical-binding: t
;; byte-compile-warnings: (not obsolete)
;; End:

;;; sclang-ac-mode.el ends here
