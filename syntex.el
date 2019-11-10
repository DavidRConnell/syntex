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

(defvar syntex-figure-extensions '("tex" "pgf" "svg" "png" "jpeg")
  "List of extensions to look for in figure directory.")

(defun syntex-insert-figure ()
  "Insert new figure with inputfigure."
  (interactive)
  (let* ((figures (syntex--strip-extensions
                   (syntex--list-figures "./figures")))
         (default (car figures))
         (figure (completing-read (concat "Figure (default "
                                               default "): ")
                                  figures))
         (macro "inputfigure"))

    (syntex--write-snippet macro figure)))

(defun syntex--list-figures (dir)
  "List all figures in figure directory DIR."
  (remove-if-not (lambda (x) (member (file-name-extension x) syntex-figure-extensions))
                 (directory-files dir)))

(defun syntex--strip-extensions (file-list)
  "Remove extensions for all files in FILE-LIST."
  (cl-loop for file-name in file-list
           collect (file-name-sans-extension file-name)))

(defun syntex--write-snippet (macro &rest arguments)
  "Insert MACRO with given ARGUMENTS into current buffer at line below point."
  (forward-line)
  (open-line 1)

  (insert (concat "\\" macro))
  (cl-loop for arg in arguments do
           (insert "{" arg "}")))

(defvar syntex-subfigure-sizes '("\\textwidth" "\\columnwidth" "\\figwidth")
  "List of default size options for inserting subfigures.")

(defun syntex-insert-subfigure ()
  "Insert subfigures with figure environment.
Keep adding figures until selected figures is not a member of figure list."
  (interactive)
  (forward-line)
  (open-line 1)
  (insert "\\begin{figure}[ht]\n\t\\centering\n\n\\end{figure}")
  (forward-line -2)

  (let* ((figures (syntex--strip-extensions
                   (syntex--list-figures "./figures")))
         (figure "")
         (sizes syntex-subfigure-sizes)
         (size "")
         (add-star-p nil)
         (star "")
         (macro "inputsubfigure"))

    (setq figures (push "" figures))
    (cl-loop do
             (setq figure (completing-read
                           "Figure (C-g to exit): "
                           figures))

             (setq figures (remove-if (lambda (x) (string= x figure))
                                      figures))

             (setq size (completing-read "Size: "
                                         sizes))

             (if (not (member size sizes))
                 (setq sizes (push size sizes)))

             (setq add-star-p (string= "yes" (completing-read
                                              "Last figure on line?: "
                                              (list "no" "yes"))))

             (if add-star-p
                 (setq star "*")
               (setq star ""))

             (syntex--write-snippet (concat macro star) size figure))))

(defun syntex-insert-table ()
  "Insert new table with inputtable."
  (interactive)
  (let* ((tables (syntex--list-tex-files "./tables"))
         (default (car tables))
         (table (completing-read (concat "Table (default "
                                               default "): ")
                                 tables))
         (macro "inputtables"))

    (syntex--write-snippet macro table)))

(defun syntex--list-tex-files (dir)
  "List all files in DIR with .tex extension after removing extension."
  (let* ((tex-extensions '("tex"))
         (files (remove-if-not (lambda (x) (member (file-extension x) tex-extensions))
                   (directory-files dir))))

    (syntex--strip-extensions files)))

(defun syntex-insert-section ()
  "Insert new section with inputsection."
  (interactive)
  (let* ((sections (syntex--list-tex-files "./sections"))
         (default (car sections))
         (section (completing-read (concat
                                    "Section name (default " default "): ")
                                   sections))
         (macro "inputsections"))

    ;; Create file in section directory if it doesn't already exist.
    (if (not (member section sections))
        (let ((file-name (concat section ".tex")))
          (save-excursion
            (find-file (concat "./sections/" file-name))
            (write-file file-name nil)
            (kill-buffer))))

    (syntex--write-snippet macro section)))

(provide 'syntex)
;;; syntex.el ends here
