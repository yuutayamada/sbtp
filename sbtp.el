;;; sbtp.el ---

;; Copyright (C) 2013 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/yuutayamada/sbtp
;; Version: 0.0.1
;; Keywords: shell, sbt, scala

;;; License:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Usage:
;; (require 'sbtp)
;; (setq sbtp-prompt-string "> ")
;; (define-key term-raw-map (kbd "C-a") sbtp-begging-of-line)

;; If you are not Emacs user, and you don't want to use C-a keybind to
;; move begging of line. You can change the keybind. For example:
;; (setq sbtp-bol-string "\C-b")
;; Note that this variable must set "\KEY" form to pass to term-raw-send-string.
;; (Can't use (kbd "C-a") form)

(require 'term)
(eval-when-compile (require 'cl))
(require 'thingatpt)
(require 'ensime)

(defvar sbtp-prompt-string "> ")
(defvar sbtp-lang (shell-command-to-string "echo -n $LANG")
  "default is LANG environment variable if you want to use Japanese language
 set this variable ja_JP.UTF-8.")

(defvar sbtp-prompt-string-end
  (let ((without-space   (loop for c in (split-string sbtp-prompt-string " ")
                               if (string< "" c)
                               collect c)))
    (nth (1- (length without-space)) without-space)))

(defvar sbtp-bol-string "\C-a"
  "Variable to pass to term-send-raw-string")

(defun sbtp-begging-of-line ()
  (interactive)
  (term-send-raw-string sbtp-bol-string)
  (when (sbtp-sbt-prompt-p)
    (goto-char (point-at-bol))
    (search-forward (concat sbtp-prompt-string-end " "))))

(defun sbtp-sbt-prompt-p ()
  (save-excursion
    (goto-char (point-at-bol))
    (if (or (looking-at sbtp-prompt-string)
            (looking-at (format "scala%s" sbtp-prompt-string)))  t nil)))

(defun sbtp-term-send-backspace ()
  (interactive)
  (when (sbtp-sbt-prompt-p)
    (when (not (equal (string-to-char ">") (char-before (- (point) 1))))
      (call-interactively 'backward-delete-char))))

(defadvice term-send-backspace
  (around ad-sbtp-sbt-send-backspace activate)
  ad-do-it
  (sbtp-term-send-backspace))

(defvar sbtp-console-buffer "*sbtp-console*")
(defvar sbtp-console nil)

(defun sbtp-start-console ()
  (interactive)
  (if (sbtp-console-live-p)
      (minibuffer-message "sbtp-console is already started")
    (if (not (equal 'scala-mode major-mode))
        (minibuffer-message "This buffer is not scala-mode")
      (sbtp-console-popup-buffer)
      (minibuffer-message "booting sbt console ... Please wait a sec")
      (start-process "emacs-sbtp-terminal"
                     (get-buffer-create sbtp-console-buffer) "/bin/sh" "-c"
                     (format "cd %s && LANG=%s sbt console"
                             (condition-case error
                                 (ensime-sbt-find-path-to-project)
                               (error default-directory))
                             sbtp-lang))
      (minibuffer-message "sbtp-console is ready!")
      (setq sbtp-console t))))

(defun sbtp-console-live-p ()
  (condition-case error
      (process-live-p (get-buffer-create sbtp-console-buffer))
    (error nil)))

(defun sbtp-console-popup-buffer (&rest body)
  (let ((original-buffer (current-buffer)))
    (pop-to-buffer (get-buffer sbtp-console-buffer))
    (switch-to-buffer sbtp-console-buffer)
    (when current-prefix-arg (erase-buffer))
    (when body (eval `(progn ,@body)))
    (recenter-top-bottom -1)
    (switch-to-buffer-other-window original-buffer)))

(defun sbtp-console-send (&optional str)
  (interactive)
  (if (not (sbtp-console-live-p))
      (sbtp-start-console)
    (let* ((kill-whole-line nil)
           (string
            (if (not (region-active-p))
                (or str (read-string "sbtp-console: "
                                        (thing-at-point 'word)))
                 (copy-region-as-kill (region-beginning) (region-end))
                 (car kill-ring)))
           (formatted-string
            (if current-prefix-arg
                (concat "\n\n:reset\n" string)
              string)))
      (sbtp-console-popup-buffer
       (process-send-string sbtp-console-buffer
                            (format "%s\n" formatted-string))))))

(defun sbtp-console-reset ()
  (interactive)
  (sbtp-console-send "\n\n:reset"))

(defun sbtp-console-send-line ()
  (interactive)
  (let* ((line (thing-at-point 'line))
         (chomp
          (lambda (str)
            (replace-regexp-in-string "\n$" "" str)))
         (delete-space
          (lambda (str)
            (replace-regexp-in-string "^ *" "" str))))
    (sbtp-console-send
     (funcall chomp (funcall delete-space line)))))

(defun sbtp-console-send-current-page ()
  (interactive)
  (sbtp-console-send (replace-regexp-in-string
                      "\n$" "" (buffer-string))))

(provide 'sbtp)

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; sbtp.el ends here
