;;; sclang-ac-mode.el --- Improved auto-complete for SuperCollider.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.2.3
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

;; 1. Make sure you've grabbed the latest copy of the supercollider emacs mode
;;    off github.
;;
;; 2. Install this package with `M-x package-install-file`
;;
;; 3. Add this mode to your sclang hooks:
;;    (add-hook 'sclang-mode-hook 'sclang-ac-mode)

;;; Code:

;;; Initialize packages.
;;; TODO: Remove if this package ever goes on MELPA.
(eval-and-compile
  (let ((package-archives '(("melpa" . "http://melpa.milkbox.net/packages/"))))
    (package-initialize)
    (unless package-archive-contents (package-refresh-contents))
    (dolist (pkg '(auto-complete dash s))
      (unless (package-installed-p pkg)
        (package-install pkg)))))

;;; ----------------------------------------------------------------------------

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
          (fmt (format "try { Emacs.message((%s).asCompileString) } {|err| err;}" expr))
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

(defun slc:request (format-string &rest args)
  "Define a blocking request to SuperCollider.
Empty responses are returned as nil."
  (let ((response
         (slc:deserialize
          (slc:blocking-eval-string
           (apply 'format format-string args)))))
    (if (and (stringp response)
             (s-blank? response))
        nil
      response)))

(defun slc:methods (class)
  "Return a list of methods implemented by CLASS."
  (unless (s-blank? class)
    (slc:request "%s.methods.collect {|m| [m.name, m.argList, m.ownerClass] }"
                 class)))

(defun slc:instance-vars (class)
  "Return a list of the instance variables of CLASS."
  (unless (s-blank? class)
    (slc:request "%s.instVarNames.collect(_.asString)" class)))

(defun slc:class-vars (class)
  "Return a list of the class variables of CLASS."
  (unless (s-blank? class)
    (slc:request "%s.classVarNames.collect(_.asString)" class)))

(defun slc:superclasses (class)
  "Return a list of superclasses for CLASS."
  (unless (s-blank? class)
    (-map 'symbol-name (slc:request "%s.superclasses" class))))

(defun slc:subclasses (class)
  "Return the direct subclasses of CLASS."
  (unless (s-blank? class)
    (-map 'symbol-name (slc:request "%s.subclasses" class))))

(defun slc:class-summary (class)
  "Return the summary for the given class."
  (unless (s-blank? class)
    (slc:request "SCDoc.documents[\"Classes/%s\"].summary" class)))

(defun slc:class-of (expr)
  "Evaluate EXPR and return the class of the result."
  (unless (s-blank? expr)
    (slc:blocking-eval-string (format "(%s).class" expr))))

(defun slc:ensure-qualified-method (method-name)
  "Ensure method-name begins with '*', '-' or '.' as expected by SuperCollider."
  (if (s-matches? (rx bol (any "." "*" "-")) method-name)
      method-name
    (s-prepend "*" method-name)))

(defun slc:ensure-non-meta-class (class)
  "Make sure that the given CLASS name is not prefixed by Meta_.
This is necessary when looking up documentation, because class
methods are actually instance methods of the meta-class."
  (s-chop-prefix "Meta_" class))

(defun slc:method-description (class method-name)
  "Describe the given method. "
  (let ((k (slc:ensure-non-meta-class class))
        (m (slc:ensure-qualified-method method-name)))
    (or (slc:request
         (concat "SCDoc.getMethodDoc(\"%s\", \"%s\")"
                 ".findChild(\\METHODBODY)"
                 ".findChild(\\PROSE).text") k m)
        (format "No description. See documentation for %s." k))))

(defun slc:method-arg-info (class method-name)
  "Get the name and description of each argument for a method. "
  (slc:request
   (concat "SCDoc.getMethodDoc(\"%s\", \"%s\")"
           ".findChild(\\METHODBODY)"
           ".findChild(\\ARGUMENTS).children.collect{|x| "
           "[x.text, x.findChild(\\PROSE).findChild(\\TEXT).text] "
           "} ")
   (slc:ensure-non-meta-class class)
   (slc:ensure-qualified-method method-name)))

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

(defvar slc:last-methods nil
  "The last list of methods for completion candidates")

(defun slc:method-args-bullets (class method-name)
  "Build a bulleted list describing a method's arguments."
  (-when-let* ((bullet "•")
               (args (slc:method-arg-info class method-name)))
    (format "\n\narguments:%s"
            (->> (--map (format "%s:\t %s" (car it) (cadr it)) args)
              (s-join (format "\n\n%s " bullet))
              (s-prepend (format "\n%s " bullet))))))

(defun slc:selected-method-doc (_)
  "Show documentation for the currently selected method in the `ac-menu'."
  (destructuring-bind (&whole it name arglist owner)
      ;; The selected candidate is the method name.
      (assoc (ac-selected-candidate) slc:last-methods)

    (message "--> Doc: %s" it)
    (s-concat
     ;; Display name.
     (format "%s.%s\n\n" owner name)
     ;; Display arglist.
     (unless (s-blank? arglist) arglist)
     ;; Display arglist details.
     (slc:method-args-bullets owner name))))

(defun* slc:method-item ((name arglist owner))
  "Return a popup item for the corresponding sclang method item."
  (list (symbol-name (eval name))
        (eval arglist)
        (slc:ensure-non-meta-class (symbol-name owner))))

(defun* slc:class-doc-subclasses (class &optional (maxlen 5))
  "Return a list of subclasses. It will be ellipsized if longer than MAXLEN"
  (let* ((bullet "\n• ")
         (subclasses (slc:subclasses class))
         ;; Show MAXLEN subclasses before ellipsizing.
         (sub-str (->> subclasses
                    (-take maxlen)
                    (s-join bullet)
                    (s-prepend bullet)))
         (sub-str (if (< maxlen (length subclasses))
                      (s-append "\n  …" sub-str)
                    sub-str)))
    (when subclasses
      (concat "\n\nsubclasses:" sub-str))))

(defun slc:class-documentation (class)
  "Format a help popup for CLASS."
  (let ((super (s-join " < " (slc:superclasses class))))
    (s-concat
     class
     ;; Summarize class.
     (-when-let (summary (slc:class-summary class))
       (concat ":\n" summary))
     ;; Display inheritance chain.
     (unless (s-blank? super)
       (format "\n\ninheritance chain:\n%s < %s" class super))
     ;; List subclasses.
     (slc:class-doc-subclasses class))))

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
                      (let ((xs (-map 'slc:method-item
                                      (slc:methods "AbstractFunction"))))
                        (setq slc:last-methods xs)
                        xs))))
    (document   . slc:selected-method-doc)
    (symbol     . "f")
    (limit      . nil)))

(ac-define-source sclang-methods
  '((candidates . (slc:logged
                    (let ((xs (-map 'slc:method-item (slc:methods slc:last-class))))
                      (setq slc:last-methods xs)
                      xs)))
    (document   . slc:selected-method-doc)
    (prefix     . ac-prefix-default)
    (symbol     . "f")
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
