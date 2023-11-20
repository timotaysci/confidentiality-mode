;;; confidentiality-mode.el --- Randomise your buffers -*- lexical-binding: t; -*-
;; Author:  Timothy Johnson
;; URL: https://github.com/timotaysci/confidentiality-mode
;; Website: http://www.timothyjohnsonsci.com
;; Created: 2023

;; Copyright 2023 Timothy Johnson
;; Package-Requires: ( (emacs "29.1") )
;; Version: 1.0
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;; Commentary:
;; This mode is written after several long train rides where I wanted to avoid
;; people looking over my shoulder. I wasn't working on anything confidential
;; but the thought of prying eyes made me feel uncomfortable. I mean, think of
;;all the excellent ideas I may have jotted down that might have been stolen!

;;This mode will randomise all text on the screen and only reveal the text in
;;the line/sentence the cursor is on.

;;Type away you crazy horses.

;;Feedback and comments very much welcome.

;;; Code:

(defvar confidentiality-mode-display-type 'sentence
  "The type of text to display.
Possible values are \\='sentence' and \\='paragraph'.")

(define-minor-mode confidentiality-mode
  "Randomize text in lines other than the current line."
  :init-value nil
  :lighter " Confidential"
  (if confidentiality-mode
      (progn
        (add-hook 'post-command-hook #'confidentiality-mode-randomize-text)
        (add-hook 'pre-command-hook #'confidentiality-mode-revert))
    (remove-hook 'post-command-hook #'confidentiality-mode-randomize-text)
    (remove-hook 'pre-command-hook #'confidentiality-mode-revert)
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
                            (confidentiality-mode-get-text-to-randomize
                             line-start line-end))))))))))

(defun confidentiality-mode-get-text-to-randomize (line-start line-end)
"Get the text to randomize. LINE-START is the start position of the line.
Argument LINE-END is the end position of the line."
  (let ((text (buffer-substring-no-properties line-start line-end)))
    (cond ((eq confidentiality-mode-display-type 'sentence)
           (confidentiality-mode-get-sentence text))
          ((eq confidentiality-mode-display-type 'paragraph)
           (confidentiality-mode-get-paragraph text))
          (t text))))

(defun confidentiality-mode-get-sentence (text)
"Get the current sentence from the given TEXT."
  (let ((sentence-start "\\([^.!?]+[.!?]\\)\\s-*")
        (sentence-end "\\([^.!?]+[.!?]\\)\\s-*"))
    (if (string-match (concat sentence-start sentence-end) text)
        (match-string 0 text)
      text)))

(defun confidentiality-mode-get-paragraph (text)
"Get the current paragraph from the given TEXT."
  (let ((paragraph-start "\\(^\\s-*$\\|[.!?]\\s-*$\\)")
        (paragraph-end "\\(^\\s-*$\\|[.!?]\\s-*$\\)"))
    (if (string-match (concat paragraph-start paragraph-end) text)
        (match-string 0 text)
      text)))

(defun confidentiality-mode-randomize-string (string)
"Randomize a given STRING by replacing each character with a random character."
  (let ((randomized-string ""))
    (dotimes (_ (length string))
      (setq randomized-string
            (concat randomized-string
                           (confidentiality-mode-random-alphanumeric-char))))
    randomized-string))

(defun confidentiality-mode-random-alphanumeric-char ()
"Generate a random alphanumeric character."
  (let* ((chars
          "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
         (random-index (random (length chars))))
    (substring chars random-index (1+ random-index))))

(defun confidentiality-mode-revert ()
"Revert all randomized text back to the original text."
  (remove-overlays (point-min) (point-max)))

(provide 'confidentiality-mode)


;;; confidentiality-mode.el ends here
