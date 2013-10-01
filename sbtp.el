
(require 'term)

(defvar sbtp-prompt-string "> ")
(defvar sbtp-prompt-string-end sbtp-prompt-string)

(defun sbtp-C-a ()
  (interactive)
  (term-send-raw-string "\C-a")
  (when (sbtp-sbt-prompt-p)
    (goto-char (point-at-bol))
    (search-forward sbtp-prompt-string-end)))

(defun sbtp-sbt-prompt-p ()
  (save-excursion
    (goto-char (point-at-bol))
    (if (looking-at sbtp-prompt-string) t nil)))

(defun sbtp-term-send-backspace ()
  (interactive)
  (when (sbtp-sbt-prompt-p)
    (when (not (equal (string-to-char ">") (char-before (- (point) 1))))
      (call-interactively 'backward-delete-char))))

(defadvice term-send-backspace
  (around ad-sbtp-sbt-send-backspace activate)
  ad-do-it
  (sbtp-term-send-backspace))

(provide 'sbtp)
