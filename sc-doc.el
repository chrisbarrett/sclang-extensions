;;; sc-doc --- SuperCollider documentation bindings.

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

;; SuperCollider documentation bindings.

;;; Code:

(autoload 'sclang-eval-string "sclang-help")

(cl-defun scd--blocking-eval-string (expr &optional (timeout-ms 100))
  "Ask SuperCollider to evaluate the given string EXPR. Wait a maximum TIMEOUT-MS."
  (let ((result nil)
        (elapsed 0)
        ;; Prevent expressions from crashing sclang.
        (fmt (format "try { Emacs.message((%s).asString) } {|err|}" expr))
        )
    ;; Stop compiler from compaining about flet.
    ;; SuperCollider will eval the string and then call back with the result.
    ;; We rebind Emacs' `message' action to intercept the response.

    (flet ((message (str &rest _) (setq result str)))

      (sclang-eval-string fmt)

      ;; Block until we receive a response or the timeout expires.
      (while (and (not result) (> timeout-ms elapsed))
        (sleep-for 0 10)
        (setq elapsed (+ 10 elapsed)))
      result)))

(provide 'sc-doc)

;; Local Variables:
;; lexical-binding: t
;; byte-compile-warnings: (not obsolete)
;; End:

;;; sc-doc.el ends here
