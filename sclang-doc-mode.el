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

;;; Code:

(defcustom sclang-doc-idle-delay 0.25
  "Delay in seconds before displaying documentation in the minibuffer."
  :group 'sclang-extensions)

;;; ----------------------------------------------------------------------------

(defun slc:show-minibuffer-doc ()
  "Display the appropriate documentation for the symbol at point."
  (message "Hello!"))

(defvar scl:doc-timer nil
  "Timer to trigger minibuffer documentation.")

(defun scl:start-timer ()
  "Activate the minibuffer doc timer."
  (scl:stop-timer)
  (setq scl:doc-timer (run-with-idle-timer sclang-doc-idle-delay 'repeat
                                           'slc:show-minibuffer-doc)))

(defun scl:stop-timer ()
  "Stop the minibuffer doc timer."
  (when scl:doc-timer
    (cancel-timer scl:doc-timer)
    (setq scl:doc-timer nil)))

(defvar sclang-doc-mode-hook)

;;;###autoload
(define-minor-mode sclang-doc-mode
  "Displays minibuffer documentation for the SuperCollider symbol at point."
  nil " doc" nil
  (cond

   ;; Enable mode.
   (sclang-doc-mode
    (scl:start-timer)
    (run-hooks 'sclang-doc-mode-hook))
   ;; Deactivate mode.
   (t
    (scl:stop-timer))))

(provide 'sclang-doc-mode)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; sclang-doc-mode.el ends here
