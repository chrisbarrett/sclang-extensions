;;; sclang-ac-mode.el --- Improved auto-complete for SuperCollider.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 0.2.3
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
(require 'auto-complete)
(autoload 'sclang-eval-string "sclang-help")
(autoload 'thing-at-point-looking-at "thingatpt")

(defgroup sclang-ac nil
  "Improved auto-complete for SuperCollider."
  :group 'languages
  :prefix "sclang-ac")

(defcustom sclang-ac-verbose
  nil
  "If non-nil, print extra debugging info to the messages buffer."
  :group 'sclang-ac)

;;; ----------------------------------------------------------------------------

(defmacro scl:logged (&rest body)
  "Like `progn', but logs the result to messages if `sclang-ac-verbose' is non-nil."
  (declare (indent 0))
  (let ((result (cl-gensym)))
    `(let ((,result (progn ,@body)))
       (when sclang-ac-verbose
         (message "[sclang-ac]: %s" ,result))
       ,result)))

(defun* scl:blocking-eval-string (expr &optional (timeout-ms 50))
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

(defun scl:deserialize (str)
  "Parse the SuperCollider response STR."
  (when str
    (->> str
      ;; Parse SuperCollider arrays to lists.
      (s-replace "," "")
      (s-replace "[" "(")
      (s-replace "]" ")")
      (read))))

(defun scl:request (format-string &rest args)
  "Define a blocking request to SuperCollider.
Empty responses are returned as nil."
  (let ((response
         (scl:deserialize
          (scl:blocking-eval-string
           (apply 'format format-string args)))))
    (if (and (stringp response)
             (s-blank? response))
        nil
      response)))

(defun scl:methods (class)
  "Return a list of methods implemented by CLASS."
  (unless (s-blank? class)
    (scl:request "%s.methods.collect {|m| [m.name, m.argList, m.ownerClass] }"
                 class)))

(defun scl:all-methods (class)
  "Return a list of methods implemented by CLASS and its superclasses."
  (unless (s-blank? class)
    (->> (cons class (scl:superclasses class))
      (-mapcat 'scl:methods)
      (-uniq))))

(defun scl:instance-vars (class)
  "Return a list of the instance variables of CLASS."
  (unless (s-blank? class)
    (scl:request "%s.instVarNames.collect(_.asString)" class)))

(defun scl:class-vars (class)
  "Return a list of the class variables of CLASS."
  (unless (s-blank? class)
    (scl:request "%s.classVarNames.collect(_.asString)" class)))

(defun scl:superclasses (class)
  "Return a list of superclasses for CLASS."
  (unless (s-blank? class)
    (-map 'symbol-name (scl:request "%s.superclasses" class))))

(defun scl:subclasses (class)
  "Return the direct subclasses of CLASS."
  (unless (s-blank? class)
    (-map 'symbol-name (scl:request "%s.subclasses" class))))

(defun scl:class-summary (class)
  "Return the summary for the given class."
  (unless (s-blank? class)
    (scl:request "SCDoc.documents[\"Classes/%s\"].summary" class)))

(defun scl:class-of (expr)
  "Evaluate EXPR and return the class of the result."
  (unless (s-blank? expr)
    (scl:blocking-eval-string (format "(%s).class" expr))))

(defun scl:ensure-non-meta-class (class)
  "Make sure that the given CLASS name is not prefixed by Meta_.
This is necessary when looking up documentation, because class
methods are actually instance methods of the meta-class."
  (s-chop-prefix "Meta_" class))

(defun scl:method-arg-info (class method-name)
  "Get the name and description of each argument for a method. "
  (let ((k (scl:ensure-non-meta-class class)))
    (or
     ;; Try class method.
     (scl:request
      (concat "SCDoc.getMethodDoc(\"%s\", \"*%s\")"
              ".findChild(\\METHODBODY)"
              ".findChild(\\ARGUMENTS).children.collect{|x| "
              "[x.text, x.findChild(\\PROSE).findChild(\\TEXT).text] "
              "} ") k method-name)
     ;; Try instance method.
     (scl:request
      (concat "SCDoc.getMethodDoc(\"%s\", \"-%s\")"
              ".findChild(\\METHODBODY)"
              ".findChild(\\ARGUMENTS).children.collect{|x| "
              "[x.text, x.findChild(\\PROSE).findChild(\\TEXT).text] " "} ")
      k method-name))))

(defun scl:all-classes ()
  "Return the list of classes known to SuperCollider."
  (->> "Class.allClasses.asArray"
      (scl:blocking-eval-string)
      (s-replace "class" "")
      (scl:deserialize)
      (-map 'symbol-name)))

(defun scl:looking-at-member-access? ()
  "Return point if not looking at a member access."
  (when (s-contains? "." (thing-at-point 'line ))
    (point)))

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

(defun* scl:ellipsize (str &optional (maxlen 30))
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

(scl:ellipsize "(the quick brown fox jumps over the lazy dog ho ho ho ho ho ho)")

(defun scl:method-bullets (method-arg-info)
  "Build a bulleted list describing a method's arguments."
  (when method-arg-info
    (format "\n\narguments:%s"
            (->> method-arg-info
              (--map (format "%s: \t%s" (car it) (cadr it)))
              (s-join (format "\n\n%s " scl:bullet))
              (s-prepend (format "\n%s " scl:bullet))))))

(defun* scl:selected-method-doc ((arglist owner)
                                 &optional (name (ac-selected-candidate)))
  "Show documentation for the currently selected method in the `ac-menu'."
  (s-concat
   ;; Display name.
   (format "%s.%s\n\n" owner name)
   ;; Display arglist.
   (unless (s-blank? arglist) arglist)
   ;; Display arglist details.
   (scl:method-bullets (scl:method-arg-info owner name))))

(defun* scl:method-item ((name arglist owner))
  "Stringify and process the elements of an sclang method item."
  (let ((sym (eval name)))
    (when (symbolp sym)
      (list (symbol-name sym)
            (eval arglist)
            (scl:ensure-non-meta-class (symbol-name owner))))))

(defun* scl:class-doc-subclasses (class &optional (maxlen 5))
  "Return a list of subclasses. It will be ellipsized if longer than MAXLEN"
  (let* ((subclasses (scl:subclasses class))
         ;; Show MAXLEN subclasses before ellipsizing.
         (sub-str (->> subclasses
                    (-take maxlen)
                    (s-join scl:bullet)
                    (s-prepend scl:bullet)))
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

(defun* scl:selected-var-doc
    (var-name &optional (class scl:last-class))
  "Get the documentation for VAR-NAME."
  (let ((qual-sym (format "%s.%s" (scl:find-declaring-class class var-name) var-name)))
    (s-concat
     ;; CLASS.NAME
     qual-sym
     ;; Show value if possible.
     (->> (scl:request "%s.%s" (scl:ensure-non-meta-class class) var-name)
       (prin1-to-string)
       (scl:ellipsize)
       (concat "\n\nvalue: ")))))

(ac-define-source sclang-classes
  '((candidates . (scl:logged
                    (unless (scl:looking-at-member-access?)
                      (scl:all-classes))))
    (document   . scl:class-documentation)
    (symbol     . "s")
    (limit      . nil)))

(ac-define-source sclang-toplevel-functions
  '((candidates . (scl:logged
                    (unless (scl:looking-at-member-access?)
                      (-map 'scl:method-item (scl:methods "AbstractFunction")))))
    (document   . scl:selected-method-doc)
    (symbol     . "f")
    (limit      . nil)))

(ac-define-source sclang-methods
  '((candidates . (scl:logged
                    (->> (scl:all-methods scl:last-class)
                      (-map 'scl:method-item)
                      (-remove 'null))))
    (document   . scl:selected-method-doc)
    (prefix     . ac-prefix-default)
    (symbol     . "f")
    (limit      . nil)
    (requires   . -1)))

(ac-define-source sclang-ivars
  '((candidates . (scl:logged
                    (scl:instance-vars scl:last-class)))
    (prefix     . ac-prefix-default)
    (document   . scl:selected-var-doc)
    (symbol     . "v")
    (limit      . nil)
    (requires   . -1)))

;;; ----------------------------------------------------------------------------

(defun scl:open-brace-pos ()
  "Find the position of the first preceding opening brace."
  (save-excursion
    (search-backward-regexp (rx (any "(" "[" "{"))
                            (line-beginning-position)
                            t)))

(defun scl:between? (n start end)
  "Non-nil if N is between START and END, inclusively."
  (and (>= n start) (<= n end)))

(defun* scl:expression-start-pos (&optional (pt (point)))
  "Return the start of the current sclang expression."
  (save-excursion
    (goto-char pt)
    (let* ((bol (line-beginning-position))
           (semicolon (save-excursion (search-backward ";" bol t)))
           (open-brace (scl:open-brace-pos))

           ;; Find the extents of the nearest preceding braced expression.
           (brace-end (save-excursion
                        (search-backward-regexp (rx (any ")" "]" "}")) bol t)))
           (brace-start (ignore-errors
                          (save-excursion
                            (goto-char brace-end)
                            (backward-sexp)
                            (point))))

           ;; Ignore semicolons that are inside braced expressions.

           (semicolon-at-this-nesting?
            (ignore-errors
              (and (> semicolon open-brace)
                   (not (scl:between? semicolon brace-start brace-end))))))
      (or
       ;; Skip backwards over braced expressions and continue.
       (when (thing-at-point-looking-at (rx (any "}" "]" ")" "\"") (* space)))
         (save-excursion
           (backward-sexp)
           (scl:expression-start-pos (point))))

       ;; Check for semicolon at this level of nesting.
       (when (and semicolon semicolon-at-this-nesting?)
         (1+ semicolon))

       ;; If we're inside a braced expression, return the start position.
       (when open-brace
         (1+ open-brace))

       ;; Otherwise, fall back to using the whole line.
       (line-beginning-position)))))

(defun scl:class-of-thing-at-point ()
  "Return the class of the sclang expression at point."
  (scl:logged
    (->> (buffer-substring-no-properties (scl:expression-start-pos) (point))
      (s-trim)
      ;; Remove trailing dot-accessor.
      (s-chop-suffix ".")
      (scl:class-of))))

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

;;;###autoload
(defun sclang-expression-start ()
  "Move to the start of the sclang expression before point."
  (interactive)
  (goto-char (scl:expression-start-pos)))

(defvar sclang-ac-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd ".") 'sclang-electric-dot)
    (define-key map (kbd "M-a") 'sclang-expression-start)
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
