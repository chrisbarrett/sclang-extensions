;;; sclang-post-mode.el --- Displays messages recieved from SuperCollider in the minibuffer.

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

;;; Displays messages recieved from SuperCollider in the minibuffer.

;;; Code:

(require 's)
(require 'dash)
(autoload 'sclang-process-filter "sclang-interp")

;;; ----------------------------------------------------------------------------

(defcustom sclang-post-message-max-lines 1
  "The maximum number of lines from the Post buffer to show in the minibuffer."
  :group 'sclang-extensions
  :type 'integer)

(defcustom sclang-post-buffer-updated-hook '(scl:print-post-message)
  "Hook that is run whenever the SuperCollider Post buffer is updated.
Hook functions should take a single argument, representing the
string that was written to the Post buffer."
  :group 'sclang-extensions
  :type 'hook)

;;; ----------------------------------------------------------------------------

(defun scl:print-post-message (str)
  "Print STR to the minibuffer if it is not blank."
  (when (and str (not (s-blank? str)))
    (message (->> (s-trim str)
               (s-lines)
               (-take sclang-post-message-max-lines)
               (s-join "\n")
               (scl:maybe-propertize)))))

(defun scl:maybe-propertize (str)
  "Propertize STR if it an error or warning."
  (cond
   ((s-starts-with? "ERROR: " str)
    (concat (propertize "ERROR: " 'face 'error)
            (s-chop-prefix "ERROR: " str)))

   ((s-starts-with? "WARNING: " str)
    (concat (propertize "WARNING: " 'face 'warning)
            (s-chop-prefix "WARNING: " str)))
   (t
    str)))

(defadvice sclang-process-filter (after scl:relay-to-hook disable)
  "Piggy-back on the sclang process filter so we can relay received messages."
  ;; The 1st arg (0-indexed) is the string received from SuperCollider.
  (run-hook-with-args 'sclang-post-buffer-updated-hook (ad-get-arg 1)))

;;;###autoload
(define-minor-mode sclang-post-mode
  "Minor-mode that displays messages from SuperCollider in the minibuffer."
  nil nil nil
  (cond
   (sclang-post-mode
    (ad-enable-advice 'sclang-process-filter 'after 'scl:relay-to-hook)
    (ad-activate 'sclang-process-filter))
   (t
    (ad-disable-advice 'sclang-process-filter 'after 'scl:relay-to-hook)
    (ad-activate 'sclang-process-filter))))

(provide 'sclang-post-mode)

;;; sclang-post-mode.el ends here
