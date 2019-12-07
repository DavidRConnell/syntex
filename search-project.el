;;; search-project.el --- Find files and keywords in project -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; Find files
(defun syntex--main-file ()
  "Find main file in project.
Main file defined as the .tex file with the same name as the project's root dir."
  (let* ((project-name (syntex--project-root))
         (file-name-san-ext (car (last (remove-if
                                        #'string-empty-p
                                        (split-string project-name "/"))))))

    (concat project-name file-name-san-ext ".tex")))

(defun syntex--project-root ()
  "Find the current project's root file.
TODO: Remove dependency on projectile."
  (projectile-project-root))

;;; Macro Matching
(defun syntex--index-last-match (index)
  "Get the value of the last regexp search at INDEX.
Striping extra white space."
  (replace-regexp-in-string
   "[\t\n ]+" " " (match-string index)))

(defun syntex--find-regexp-in-project (regexp index)
  "Search project for the INDEXth subexpression of REGEXP.
FIXME: Only searches current file; fix to search entire project."
  (save-excursion
    (goto-char (point-min))
    (cl-loop while (search-forward-regexp regexp nil t)
             collect (syntex--index-last-match index))))

;;; Citations
(defvar syntex-bib-regex (concat "^@\\([a-zA-Z0-9]*\\){"
                                 "\\([^,]*\\),\n"
                                 "[ \t]*author[ =]*{\\([^}]*\\)},\n"
                                 "[ \t]*title[ =]*{\\([^}]*\\)},\n"
                                 "[ \t]*journal[ =]*{\\([^}]*\\)}"))
(defun syntex-cite ()
  "Completion read for inserting citation from bib file in \bibliography{} call in main."
  (interactive)
  (let* ((reference-alist (syntex--parse-bib-file))
         (citation (completing-read "Citation title: "
                   (mapcar #'car reference-alist))))

    (forward-line 1)
    (insert "\\cite{" (second (assoc citation reference-alist)) "}")))

(defun syntex--parse-bib-file ()
  "Get bib file entries for completion-read."
  (with-temp-buffer
    (insert-file-contents (syntex--bib-file))
    (cl-loop while (search-forward-regexp syntex-bib-regex nil t)
             collect (list (syntex--index-last-match 4) (syntex--index-last-match 2)))))

(defun syntex--bib-file ()
  "Find the bib file used in called in the main file."
  (with-temp-buffer
    (insert-file-contents (syntex--main-file))
    (search-forward-regexp "^[ \t]*\\\\bibliography{\\([^}]*\\)}" nil t)
    (concat (syntex--index-last-match 1) ".bib")))

(provide 'search-project)
;;; search-project.el ends here
