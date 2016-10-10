;;; ob-sclang.el --- SCLang support for Org-mode Babel
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:
;;; ----------------------------------------------------------------------------
(require 'org)
(require 'ob)

(require 'sclang-interp)

(defgroup ob-sclang nil
  "org-mode blocks for SuperCollider SCLang."
  :group 'org)

;;;###autoload
(defun org-babel-execute:sclang (body params)
  "Org-mode Babel sclang hook for evaluate `BODY' with `PARAMS'."
  (unless (or (equal (buffer-name) sclang-post-buffer)
              (sclang-get-process))
    (sclang-start))
  
  ;; (let* ((db (or (cdr (assoc :db params))
  ;;                ob-mongo:default-db))
  ;;        (cmd (mapconcat 'identity (list "mongo" "--quiet" db) " ")))
  ;;   (org-babel-eval cmd body))
  
  (sclang-eval-string body t)

  ;; (let ((cmd "sclang -r -s -D"))
  ;;   (org-babel-eval cmd body))
  )

(defvar org-babel-default-header-args:sclang nil)

(setq org-babel-default-header-args:sclang
      '((:session . "*SCLang:Workspace*")
        (:output . "none")) ; TODO: temporary can't find way to let sclang output to stdout for org-babel.
      )

;;;###autoload
(with-eval-after-load "org"
  (add-to-list 'org-src-lang-modes '("sclang" . sclang)))

;;; ----------------------------------------------------------------------------

(provide 'ob-sclang)

;;; ob-sclang.el ends here
