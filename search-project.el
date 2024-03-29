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

(defvar syntex-tex-directories '("" "sections" "appendices")
  "List of directories to search in for .tex files relative to root dir.")

(defun syntex--regexp-search-tex-files (regexp index)
  "Search project for the INDEXth subexpression of REGEXP.
Only searches tex files under directories in syntex-tex-directories list."
  (let* ((results '()))
    (cl-loop for dir in syntex-tex-directories do
             (let ((absdir (concat (syntex--project-root) dir "/")))
               (cl-loop for file in (syntex--list-tex-files absdir) do
                        (setq results (nunion results (syntex--search-file
                                        (concat absdir file ".tex") regexp index))))))
    results))

(defun syntex--search-file (file regexp index)
  "Search FILE for the INDEXth subexpression of REGEXP."
  (with-temp-buffer
    (insert-file-contents file)
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

    (forward-char 1)
    (insert "\\cite{" (second (assoc citation reference-alist)) "}")))

(defun syntex--parse-bib-file ()
  "Get bib file entries for completion-read."
  (with-temp-buffer
    (insert-file-contents (syntex--bib-file))
    (cl-loop while (search-forward-regexp syntex-bib-regex nil t)
             collect (list (syntex--index-last-match 4) (syntex--index-last-match 2)))))

(defun syntex--bib-file ()
  "Find the bib file called in the main file.
TODO: Extend to work with multiple bib files"
  (with-temp-buffer
    (insert-file-contents (syntex--main-file))
    (search-forward-regexp "^[ \t]*\\\\bibliography{\\([^}]*\\)}" nil t)
    (concat (syntex--index-last-match 1) ".bib")))

(provide 'search-project)
;;; search-project.el ends here
