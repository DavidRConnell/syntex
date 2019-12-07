;;; syntex.el --- Advanced snippet editing for latex mode -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'popup)
(require 'subr-x)
(require 'cl-seq)
(require 'userinput "~/projects/syntex/userinput.el")
(require 'search-project "~/projects/syntex/search-project.el")

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

(defvar syntex-complete-inserted-elements nil
  "If non-nil do include already used elements (figures, sections, etc) in completion.")

(defun syntex-insert-figure (no-opt-p)
  "Insert new figure with inputfigure.
If 'universal-arg' NO-OPT-P is non-nil do not ask for optional value."
  (interactive "P")
  (let* ((figures (syntex--find-figures-for-completion))
         (figure (completing-read "Figure: "
                                  figures))

         (caption (syntex--get-optional-if-arg (not no-opt-p)
                                               (concat "Caption for "
                                                       figure ":")))
         (macro "\\inputfigure"))

    (syntex--write-snippet macro caption figure)))

(defvar syntex--figure-name-regex (concat
                                   "\\\\input\\(sub\\)?figure\\({[^}]*}\\)?{\\([^}]*\\)}"))

(defun syntex--find-figures-for-completion ()
  "Return figures for completion.
If `syntex-complete-inserted-elements' is non-nil return all figures; otherwise,
return only those that have not yet been used in the project."
  (let ((figures (syntex--strip-extensions
                  (syntex--list-figures "./figures"))))

       (if syntex-complete-inserted-elements
           figures
         (remove-if (lambda (figure)
                      (member figure
                              (syntex--find-regexp-in-project
                               syntex--figure-name-regex 3)))
                    figures))))

(defun syntex--list-figures (dir)
  "List all figures in figure directory DIR."
  (remove-if-not (lambda (x) (member (file-name-extension x) syntex-figure-extensions))
                 (directory-files dir)))

(defun syntex--strip-extensions (file-list)
  "Remove extensions for all files in FILE-LIST."
  (cl-loop for file-name in file-list
           collect (file-name-sans-extension file-name)))

(defun syntex--get-optional-if-arg (arg &optional prompt)
  "Prompt user to enter an optional value.
If 'universal-arg' ARG is nil skip prompt and return an empty string.
Optional PROMPT provides instructions to the top of the input buffer."
  (if arg
      (syntex--read-from-tmp-buffer prompt)
    ""))

(defun syntex--write-snippet (macro optional &rest arguments)
  "Insert MACRO with given ARGUMENTS into current buffer at line below point.
Add OPTIONAL value if not empty."
  (forward-line)
  (open-line 1)

  (insert macro)

  (if (not (string-empty-p optional))
      (insert "[" optional "]"))
  (cl-loop for arg in arguments do
           (insert "{" arg "}")))

(defvar syntex-subfigure-sizes '("\\textwidth" "\\columnwidth" "\\figwidth")
  "List of default size options for inserting subfigures.")

(defun syntex-insert-subfigure (no-opt-p)
  "Insert subfigures with figure environment.
Keep adding figures until selected figures is not a member of figure list.
If the 'universal-arg' NO-OPT-P is non-nil do not prompt for captions."
  (interactive "P")
  (forward-line)
  (open-line 1)
  (insert "\\begin{figure}[ht]\n\t\\centering\n\n\\end{figure}")
  (forward-line -1)

  (let* ((figures (syntex--find-figures-for-completion))
         (figure "")
         (sizes syntex-subfigure-sizes)
         (size "")
         (add-star-p nil)
         (star "")
         (macro "\t\\inputsubfigure")
         (maincaption (syntex--get-optional-if-arg
                       (not no-opt-p)
                       "Caption for entire set of figures:")))

    (insert "\t\\caption{" maincaption "}")
    (forward-line -1)
    (setq figures (push "" figures))
    (cl-loop do
             (setq figure (completing-read
                           "Figure (C-g to exit): "
                           figures))

             (setq figures (remove-if (lambda (x) (string= x figure))
                                      figures))

             (setq subfigurecaption (syntex--get-optional-if-arg (not no-opt-p)
                                                                 (concat "Caption for "
                                                                         figure ":")))

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

             (syntex--write-snippet (concat macro star) subfigurecaption size figure))))

(defun syntex-insert-table (no-opt-p)
  "Insert new table with inputtable.
If 'universal-arg' NO-OPT-P is non-nil do not prompt for caption."
  (interactive "P")
  (let* ((tables (syntex--list-tex-files "./tables"))
         (default (car tables))
         (table (completing-read (concat "Table (default "
                                               default "): ")
                                 tables))
         (caption (syntex--get-optional-if-arg (not no-opt-p)
                                               (concat "Caption for "
                                                       table ":")))
         (macro "\\inputtables"))

    (syntex--write-snippet macro caption table)))

(defun syntex--list-tex-files (dir)
  "List all files in DIR with .tex extension after removing extension."
  (let* ((tex-extensions '("tex"))
         (files (remove-if-not (lambda (x) (member (file-name-extension x) tex-extensions))
                   (directory-files dir))))

    (syntex--strip-extensions files)))

(defun syntex--insert-section-type (no-opt-p macro dir)
  "Templaet function for inserting sections, subsections, and appendices.
MACRO is the macro to insert and DIR is the location the file should be.
If 'universal-argument' NO-OPT-P is non-nil do not pormpt for optional name."
  (let* ((files (syntex--list-tex-files (concat "./" dir)))
         (file (completing-read "File name: " files))
         (display-name (syntex--get-optional-if-arg
                        (not no-opt-p)
                        (concat "Optional header (default if blank `"
                                (capitalize file) "') or enter nil for no header:"))))

    ;; Create file in directory DIR if it doesn't already exist.
    (if (not (member file files))
        (let ((file-name (concat file ".tex")))
          (save-excursion
            (find-file (concat "./" dir "/" file-name))
            (write-file file-name nil)
            (kill-buffer))))

    (if (string-match display-name "nil")
        (progn
          (setq display-name "")
          (setq macro (concat macro "*"))))
    (syntex--write-snippet macro display-name file)))

(defun syntex-insert-section (no-opt-p)
  "Insert new section with inputsection.
If 'universal-arg' NO-OPT-P non-nil do not prompt for optional section name."
  (interactive "P")
  (syntex--insert-section-type no-opt-p "\\inputsection" "sections"))

(defun syntex-insert-subsection (no-opt-p)
  "Insert new subsection with inputsubsection.
If 'universal-arg' NO-OPT-P non-nil do not prompt for optional section name."
  (interactive "P")
  (syntex--insert-section-type no-opt-p "\\inputsubsection" "sections"))

(defun syntex-insert-appendix (no-opt-p)
  "Insert new appendix with inputappendix.
If 'universal-arg' NO-OPT-P non-nil do not prompt for optional section name."
  (interactive "P")
  (syntex--insert-section-type no-opt-p "\\inputappendix" "appendices"))

(provide 'syntex)
;;; syntex.el ends here
