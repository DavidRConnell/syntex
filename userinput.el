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

(defun syntex--read-from-tmp-buffer (&optional prompt)
  "Open a temporary buffer in a pop-up window to prompt for input from the user.
Present user with optional read only PROMPT at top of buffer."
  (if prompt
      (setq prompt (concat prompt "\n\n"))
    (setq prompt ""))

  (let ((tmp-buffer (get-buffer-create "*User Input*"))
        input)

    (unwind-protect
        (if (catch 'save-p
              (pop-to-buffer tmp-buffer)
              (syntex--setup-user-input-buffer prompt)
              (recursive-edit))
            (setq input (buffer-substring (+ (length prompt) 1) (point-max)))
          (setq input ""))
      (kill-buffer))
    input))

(defun syntex--setup-user-input-buffer (prompt)
  "Setup the user input buffer with read-only PROMPT."
  (funcall #'syntex--user-input-mode)

  (insert prompt)
  (add-face-text-property (point-min) (length prompt)
                          '(:weight bold :inherit font-lock-keyword-face))
  (put-text-property (point-min) (length prompt) 'read-only t))

(defun syntex--add-input ()
  "Return control to main control loop."
  (interactive)
  (throw 'save-p t)
  (exit-recursive-edit))

(defun syntex--no-input ()
  "Erase buffer then returen control to main control loop."
  (interactive)
  (throw 'save-p nil)
  (exit-recursive-edit))

(provide 'userinput)
;;; userinput.el ends here
