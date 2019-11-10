;;; userinput.el --- Get user input -*- lexical-binding: t; -*-
;;; Commentary:
;; Add methods for getting user input from a temporary buffer. Used instead of
;; normal read-from-minibuffer to allow for more complex editing including
;; citation and referencing functions.
;;; Code:

(define-minor-mode syntex--user-input-mode
  "Minor mode for inserting optionals to latex macros."
  nil " User Input" syntex--user-input-mode-map
  (setq-local
   header-line-format
   (substitute-command-keys
    "\\<syntex--user-input-mode-map>Accept: `\\[syntex--add-input]', \
Reject (return empty string): `\\[syntex--no-input]' or leave blank and finish")))

(defvar syntex--user-input-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" #'syntex--add-input)
    (define-key map "\C-c\C-k" #'syntex--no-input)
    map))

(defun syntex--read-from-tmp-buffer ()
  "Open a temporary buffer in a pop-up window to prompt for input from the user."

  (let ((tmp-buffer (get-buffer-create "*User Input*"))
        input)
    (unwind-protect
        (progn
          (pop-to-buffer tmp-buffer)
          (funcall #'LaTeX-mode)
          (funcall #'syntex--user-input-mode)
          (recursive-edit)
          (setq input (buffer-string)))
      (kill-buffer))
    input))

(defun syntex--add-input ()
  "Return control to main control loop."
  (interactive)
  (exit-recursive-edit))

(defun syntex--no-input ()
  "Erase buffer then returen control to main control loop."
  (interactive)
  (erase-buffer)
  (exit-recursive-edit))

(provide 'userinput)
;;; userinput.el ends here
