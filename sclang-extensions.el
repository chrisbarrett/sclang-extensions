;;; sclang-extensions.el --- Extensions for the SuperCollider Emacs mode.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Version: 2.2.1
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

(defgroup sclang-extensions nil
  "Extensions to the SuperCollider (sclang) Emacs mode."
  :group 'languages)

(defvar sclang-extensions-mode-hook)

;;;###autoload
(define-minor-mode sclang-extensions-mode
  "Enable all extensions to the sclang Emacs mode."
  nil nil nil
  (cond

   ;; Enable mode.
   (sclang-extensions-mode
    (sclang-ac-mode +1)
    (sclang-doc-mode +1)
    (run-hooks 'sclang-extensions-mode-hook))

   ;; Disable mode.
   (t
    ;; Deactivate minor modes.
    (sclang-ac-mode -1)
    (sclang-doc-mode -1))))

(provide 'sclang-extensions)

;; Local Variables:
;; lexical-binding: t
;; End:

;;; sclang-extensions.el ends here
