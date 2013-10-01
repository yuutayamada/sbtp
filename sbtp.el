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

(require 'term)

(defvar sbtp-prompt-string "> ")
(defvar sbtp-prompt-string-end sbtp-prompt-string)

(defun sbtp-begging-of-line ()
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

;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; End:

;;; sbtp.el ends here
