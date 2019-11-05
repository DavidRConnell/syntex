;;; syntex.el --- Advanced snippet editing for latex mode -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(defun syntex-insert-emph ()
  "Insert emph snippet."
  (interactive)
  (insert "\\emph{}"))

(defun syntex-insert-bold ()
  "Insert bold snippet."
  (interactive)
  (insert "\\textbf{}"))

(defun syntex-insert-italic ()
  "Insert bold snippet."
  (interactive)
  (insert "\\textit{}"))

(defun syntex-insert-label ()
  "Insert bold snippet."
  (interactive)
  (insert "\\label{}"))

(provide 'syntex)
;;; syntex.el ends here
