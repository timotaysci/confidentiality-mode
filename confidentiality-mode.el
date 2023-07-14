(defvar confidentiality-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r") 'confidentiality-mode-revert)
    map)
  "Keymap for `confidentiality-mode'.")

(defvar confidentiality-mode-display-type 'sentence
  "The type of text to display. Possible values are 'sentence' and 'paragraph'.")

(define-minor-mode confidentiality-mode
  "Randomize text in lines other than the current line."
  :init-value nil
  :lighter " Confidential"
  :keymap confidentiality-mode-map
  (if confidentiality-mode
      (progn
        (add-hook 'post-command-hook 'confidentiality-mode-randomize-text)
        (add-hook 'pre-command-hook 'confidentiality-mode-revert))
    (remove-hook 'post-command-hook 'confidentiality-mode-randomize-text)
    (remove-hook 'pre-command-hook 'confidentiality-mode-revert)
    (confidentiality-mode-revert)))

(defun confidentiality-mode-randomize-text ()
  "Randomize text in lines other than the current line."
  (unless (minibufferp)
    (let ((current-line (line-number-at-pos))
          (start (point-min))
          (end (point-max)))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (forward-line)
          (unless (= (line-number-at-pos) current-line)
            (let ((line-start (line-beginning-position))
                  (line-end (line-end-position)))
              (overlay-put (make-overlay line-start line-end) 'display
                           (confidentiality-mode-randomize-string
                            (confidentiality-mode-get-text-to-randomize line-start line-end))))))))))

(defun confidentiality-mode-get-text-to-randomize (line-start line-end)
  "Get the text to randomize based on the `confidentiality-mode-display-type' variable."
  (let ((text (buffer-substring-no-properties line-start line-end)))
    (cond ((eq confidentiality-mode-display-type 'sentence)
           (confidentiality-mode-get-sentence text))
          ((eq confidentiality-mode-display-type 'paragraph)
           (confidentiality-mode-get-paragraph text))
          (t text))))

(defun confidentiality-mode-get-sentence (text)
  "Get the current sentence from the given text."
  (let ((sentence-start "\\([^.!?]+[.!?]\\)\\s-*")
        (sentence-end "\\([^.!?]+[.!?]\\)\\s-*"))
    (if (string-match (concat sentence-start sentence-end) text)
        (match-string 0 text)
      text)))

(defun confidentiality-mode-get-paragraph (text)
  "Get the current paragraph from the given text."
  (let ((paragraph-start "\\(^\\s-*$\\|[.!?]\\s-*$\\)")
        (paragraph-end "\\(^\\s-*$\\|[.!?]\\s-*$\\)"))
    (if (string-match (concat paragraph-start paragraph-end) text)
        (match-string 0 text)
      text)))

(defun confidentiality-mode-randomize-string (string)
  "Randomize a given string by replacing each character with a random alphanumeric character."
  (let ((randomized-string ""))
    (dotimes (_ (length string))
      (setq randomized-string (concat randomized-string (confidentiality-mode-random-alphanumeric-char))))
    randomized-string))

(defun confidentiality-mode-random-alphanumeric-char ()
  "Generate a random alphanumeric character."
  (let* ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
         (random-index (random (length chars))))
    (substring chars random-index (1+ random-index))))

(defun confidentiality-mode-revert ()
  "Revert all randomized text back to the original text."
  (remove-overlays (point-min) (point-max)))

(provide 'confidentiality-mode)
